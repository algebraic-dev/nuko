module Nuko.Typer.Match (
  checkUseful,
  addRow,
  matrixFromColumn,
  toMatchPat,
  isExhaustive,
  checkPatterns
) where

-- The Pattern matching analysis is part of the type checker.

import Nuko.Names          (Path, Name, ConsName, Qualified (..), TyName, mkQualifiedPath, getPathInfo)
import Nuko.Typer.Tree     ()
import Nuko.Typer.Env      (MonadTyper, qualifyPath, tsConstructors, globalTypingEnv, tsTypes, SumTyInfo(..), TyInfoKind (..), TyInfo(..), DataConsInfo(..), emitDiagnostic)
import Nuko.Tree.Expr      (Pat (..))
import Nuko.Tree           (Tc)
import Nuko.Report.Range   (Range(..), HasPosition (getPos))

import Relude              (snd, (<$>), Int, concatMap, error, Maybe (..), Foldable (..), traverse, uncurry, not, fromMaybe, splitAt, Num ((-)), Text, One (one))
import Relude.Bool         (Bool(..), (||), otherwise, when)
import Relude.Base         (Eq((==)))
import Relude.List         (NonEmpty (..), replicate, filter)
import Relude.Monoid       (Semigroup((<>)))
import Relude.Function     (($), (.))
import Relude.Applicative  (pure)

import Lens.Micro.Platform (use, at)
import Data.List           (nub, all, findIndex, (!!), take)
import Pretty.Format       (Format(..))

import qualified Data.Text    as Text
import qualified Data.HashSet as HashSet
import Control.Monad (foldM)
import Nuko.Report.Message (Severity(..))
import Nuko.Typer.Error (TypeError(..))
import Data.List.NonEmpty ((<|))

-- Implementation of http://moscova.inria.fr/%7Emaranget/papers/warn/warn.pdf
-- But modified to look like the rust version.

type ConsNm = Path (Name ConsName)

data At = At Range | Created

data Match
  = Wild At
  | Or Match Match At
  | Cons ConsNm [Match] At

newtype Matrix = Matrix { unpackMatrix ::  [[Match]] }

data Witness
  = Witness (Maybe [Match])
  | NoMatter Bool

instance Format Match where
  format = \case
    Wild _ -> "_"
    Or a b _ -> "(" <> format a <> "|" <> format b <> ")"
    Cons t [] _ -> "(" <> format t <> ")"
    Cons t e _ -> "(" <> format (getPathInfo t) <> " " <> Text.intercalate " " (format <$> e) <> ")"

instance Format Matrix where
  format (Matrix []) = "||"
  format (Matrix x) = Text.intercalate "\n" ((\t -> "| " <> t <> " |") . Text.intercalate ", " <$> ((format <$>) <$> x))

instance Format Witness where
  format (NoMatter True)      = "<Yes>"
  format (NoMatter False)     = "<No>"
  format (Witness Nothing)    = "<Exhaustive>"
  format (Witness (Just res)) = "<" <> Text.intercalate ", " (format <$> res) <> ">"

toMatchPat :: Pat Tc -> Match
toMatchPat = \case
  PCons name pats (_, r) -> Cons name (toMatchPat <$> pats) (At r)
  PWild x       -> Wild (At (snd x))
  PId n _       -> Wild (At $ getPos n)
  PLit _ _      -> error "Not implemented yet"
  PAnn t _ _    -> toMatchPat t
  POr p q (_,x) -> Or (toMatchPat p) (toMatchPat q) (At x)

matrixFromColumn :: [Pat Tc] -> Matrix
matrixFromColumn pats = Matrix ((: []) . toMatchPat <$> pats)

specialize :: Path (Name ConsName) -> Int -> Matrix -> Matrix
specialize cons size (Matrix rows) = Matrix $ concatMap getRow rows where
  getRow = \case
    []               -> error "Should not be empty?"
    (Or p q _: rest) -> unpackMatrix $ specialize cons size (Matrix [p:rest, q:rest])
    (Wild _ : rest)  -> [replicate size (Wild Created) <> rest]
    (Cons path pats _ : rest)
      | path == cons -> [pats <> rest]
      | otherwise    -> []

getUsedCons :: Matrix -> [ConsNm]
getUsedCons (Matrix patMatrix) = nub $ concatMap withRow patMatrix where
  withRow = \case
    Wild _:_     -> []
    Or p q _:_   -> withRow [p] <> withRow [q]
    Cons c _ _:_ -> [c]
    []           -> []

ensure :: Maybe a -> a
ensure (Just n) = n
ensure Nothing  = error "Compiler error: Constructor does not exists!"

getConsInfo  :: MonadTyper m => ConsNm -> m DataConsInfo
getConsInfo name = do
  qualPath   <- qualifyPath name
  snd . ensure <$> use (globalTypingEnv . tsConstructors . at qualPath)

getConsFromTy :: MonadTyper m => Qualified (Name TyName) -> m TyInfo
getConsFromTy name = snd . ensure <$> use (globalTypingEnv . tsTypes . at name)

addRow :: Matrix -> Pat Tc -> Matrix
addRow (Matrix c) p = Matrix (c <> [[toMatchPat p]])

getAllCons :: MonadTyper m => ConsNm -> m (NonEmpty (Qualified (Name ConsName), Int))
getAllCons name = do
  info  <- getConsInfo name
  tInfo <- getConsFromTy info._tyName
  case tInfo._tyKind of
    IsSumType prod -> pure prod._stiConstructors
    _ -> error "Cannot match on this type!"

defaultMatrix :: Matrix -> Matrix
defaultMatrix (Matrix s) = Matrix $ concatMap getRow s where
  getRow = \case
    []             -> error "Checker Error: Should not have empty rows in default matrix"
    Cons {}  :_    -> []
    Wild _   :rest -> [rest]
    Or p q _ :rest -> unpackMatrix $ defaultMatrix (Matrix [p:rest, q:rest])

mkUseless :: Bool -> Witness
mkUseless cond = if cond then Witness Nothing else NoMatter False

mkUseful :: Bool -> Witness
mkUseful cond = if cond then Witness (Just []) else NoMatter True

extend :: Witness -> Witness -> Witness
extend (NoMatter a) (NoMatter b)      = NoMatter $ a || b
extend (Witness Nothing) (Witness  _) = Witness Nothing
extend (Witness _) (Witness Nothing)  = Witness Nothing
extend (Witness (Just a)) (Witness (Just b)) = Witness (Just $ a <> b)
extend _ _                            = error "Like.. it's not possible right now"

witnessUseful :: Witness -> Bool
witnessUseful (NoMatter res)    = res
witnessUseful (Witness Nothing) = True
witnessUseful (Witness _)       = False

specializeUseful :: MonadTyper m => Bool -> Matrix -> [Match] -> Qualified (Name ConsName) -> Int -> m Witness
specializeUseful retWitness patMatrix rest name size =
  isUseful retWitness
    (specialize (mkQualifiedPath name) size patMatrix)
    (replicate size (Wild Created) <> rest)

isUseful :: MonadTyper m => Bool -> Matrix -> [Match] -> m Witness
isUseful retWitness (Matrix [])              [] = pure $ mkUseful retWitness
isUseful retWitness (Matrix x) [] | all null x  = pure $ mkUseless retWitness
isUseful _          (Matrix _)               [] = error "Oh no!"

isUseful retWitness patMatrix (Or p q _ : rest) = do
  res1 <- isUseful retWitness patMatrix (p:rest)
  res2 <- isUseful retWitness patMatrix (q:rest)
  pure (extend res1 res2)

isUseful retWitness patMatrix (Cons t p _ : rest) = do
  cInfo <- getConsInfo t
  isUseful retWitness (specialize t cInfo._parameters patMatrix) (p <> rest)

isUseful retWitness patMatrix (Wild _ : rest) = do
  let used = getUsedCons patMatrix
  case used of
    (x : _) -> do
      all' <- getAllCons x
      if length all' == length used
        then completeWitness (toList all')
        else notCompleteWitness used (toList all')
    [] -> notCompleteWitness used []
  where
    mkCons :: (Qualified (Name ConsName), Int) -> Match
    mkCons (name, size) = Cons (mkQualifiedPath name) (replicate size (Wild Created)) Created

    notCompleteWitness used all' = do
      witness <- isUseful retWitness (defaultMatrix patMatrix) rest
      case (witness, used) of
        (Witness (Just pats), []) -> pure (Witness (Just (Wild Created : pats)))
        (Witness (Just pats),  _) -> do
          let hashUsed = HashSet.fromList used
          let notUsed = take 3 $ filter (\(name, _) -> not $ HashSet.member (mkQualifiedPath name) hashUsed) all'
          case notUsed of
            [] -> error "Compiler error: Probably it should not be empty? anyways it happened on the pattern match checking."
            (x:xs) ->
              let orMatch = foldr (\cons match -> Or (mkCons cons) match Created) (mkCons x) xs in
              pure (Witness (Just $ orMatch : pats))
        _ -> pure witness

    completeWitness all' = do
      witnessList <- traverse (uncurry (specializeUseful retWitness patMatrix rest)) all'
      let idx     = fromMaybe (length witnessList - 1) (findIndex (not . witnessUseful) (toList witnessList))
      let witness = toList witnessList !! idx
      let (name, size) = all' !! idx
      case witness of
        Witness (Just pats) -> do
          let (args, ret) = splitAt size pats
          pure $ Witness (Just (Cons (mkQualifiedPath name) args Created : ret))
        _ -> pure witness

checkUseful :: MonadTyper m => Matrix -> Pat Tc -> m Bool
checkUseful pats pat = witnessUseful <$> isUseful False pats [toMatchPat pat]

isExhaustive :: MonadTyper m => [Pat Tc] -> m Witness
isExhaustive pats = isUseful True (matrixFromColumn pats) [Wild Created]

desestructOr :: Match -> (NonEmpty Text)
desestructOr (Or a b _) = format a <| desestructOr b
desestructOr alt        = one (format alt)

checkPatterns :: MonadTyper m => Range -> (NonEmpty (Pat Tc)) -> m ()
checkPatterns range (x :| xs) = do
    let emptyMatrix = Matrix [[toMatchPat x]]
    finalMatrix    <- foldM go emptyMatrix xs
    witness        <- isUseful True finalMatrix [Wild Created]
    case witness of
      Witness (Just (match:_)) -> emitDiagnostic Error (NotExhaustive range (desestructOr match)) range
      Witness Nothing         -> pure ()
      _ -> error "Compiler error: Check patterns returned a strange result!"

  where
    go :: MonadTyper m => Matrix -> Pat Tc -> m Matrix
    go matrix@(Matrix list) pat = do
      let match = toMatchPat pat
      result <- isUseful False matrix [match]

      when (not $ witnessUseful result) $ do
        emitDiagnostic Warning (UselessClause (getPos pat)) (getPos pat)

      pure (Matrix (list <> [[match]]))
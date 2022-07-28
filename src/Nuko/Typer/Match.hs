module Nuko.Typer.Match where

-- The Pattern matching analysis is part of the type checker.

import Nuko.Names ( Path, Name, ConsName, Qualified (..), TyName, mkQualifiedPath )
import Nuko.Typer.Tree ()
import Nuko.Typer.Env (MonadTyper, qualifyPath, tsConstructors, globalTypingEnv, tsTypes, SumTyInfo(..), TyInfoKind (..), TyInfo(..), DataConsInfo(..))
import Nuko.Tree.Expr (Pat (..))
import Nuko.Tree (Tc)
import Nuko.Report.Range (Range(..), HasPosition (getPos))

import Relude (snd, (<$>), Int, concatMap, error, Maybe (..), Foldable (length), traverse, uncurry, not, putTextLn, traceM, ToString (toString))
import Relude.Bool (Bool(..), (||))
import Relude.Base (Eq((==)))
import Relude.Bool (otherwise)
import Relude.List ( NonEmpty, replicate )
import Relude.Monoid ( Semigroup((<>)) )
import Relude.Function ( ($), (.) )
import Relude.Applicative (pure, (<*>))

import Lens.Micro.Platform (use, at)
import Data.List (nub, or)
import Pretty.Format (Format(..))

import qualified Data.Text as Text

-- Implementation of http://moscova.inria.fr/%7Emaranget/papers/warn/warn.pdf
-- But modified to look like the rust version.

type ConsNm = Path (Name ConsName)

data Witness
  = NoMatter Bool
  | With [Witness]


data At = At Range | Created

data Match
  = Wild At
  | Or Match Match At
  | Cons ConsNm [Match] At

newtype Matrix = Matrix { unpackMatrix ::  [[Match]] }

instance Format Match where
  format = \case
    Wild _ -> "_"
    Or a b _ -> "(" <> format a <> "|" <> format b <> ")"
    Cons t [] _ -> "(" <> format t <> ")"
    Cons t e _ -> "(" <> format t <> " " <> Text.intercalate " " (format <$> e) <> ")"

instance Format Matrix where
  format (Matrix []) = "||"
  format (Matrix x) = Text.intercalate "\n" ((\t -> "| " <> t <> " |") <$> Text.intercalate ", " <$> ((format <$>) <$> x))

toMatchPat :: Pat Tc -> Match
toMatchPat = \case
  PCons name pats (_, r) -> Cons name (toMatchPat <$> pats) (At r)
  PWild x       -> Wild (At (snd x))
  PId n _       -> Wild (At $ getPos n)
  PLit _ _      -> error "Not implemented yet"
  PAnn t _ _    -> toMatchPat t
  POr p q (_,x) -> Or (toMatchPat p) (toMatchPat q) (At x)

matrixFromColumn :: [Pat Tc] -> Matrix
matrixFromColumn pats = Matrix (((: []) . toMatchPat) <$> pats)

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
    Cons _ _ _:_   -> []
    Wild _   :rest -> [rest]
    Or p q _ :rest -> unpackMatrix $ defaultMatrix (Matrix [p:rest, q:rest])

isUseful :: MonadTyper m => Matrix -> [Match] -> m Bool
isUseful (Matrix [])              _    = pure True
isUseful (Matrix [[]])            []   = pure False
isUseful (Matrix x)               []   = error "Oh no!"
isUseful patMatrix (Or p q _ : rest)   = (||) <$> isUseful patMatrix (p:rest) <*> isUseful patMatrix (q:rest)

isUseful patMatrix (Cons t p _ : rest) = do
  cInfo <- getConsInfo t
  isUseful (specialize t cInfo._parameters patMatrix) (p <> rest)

isUseful patMatrix (Wild _ : rest) = do
  let used = getUsedCons patMatrix
  putTextLn $ "Matrix:\n" <> format patMatrix
  case used of
    (x : _) -> do
      all <- getAllCons x
      case length all == length used of
        True  -> do
          let run name size = isUseful (specialize (mkQualifiedPath name) size patMatrix) (replicate size (Wild Created) <> rest)
          or <$> traverse (uncurry run) all
        False -> isUseful (defaultMatrix patMatrix) rest
    [] -> do
      isUseful (defaultMatrix patMatrix) rest

checkUseful :: MonadTyper m => Matrix -> Pat Tc -> m Bool
checkUseful pats pat = isUseful pats [toMatchPat pat]

isExhaustive :: MonadTyper m => [Pat Tc] -> m Bool
isExhaustive pats = not <$> isUseful (matrixFromColumn pats) [Wild Created]
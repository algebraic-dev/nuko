module Nuko.Typer.Match (
  getExhaustivenessWitness,
  checkPatterns
) where

-- Simple implementation of algorithm 𝓤' of the paper
-- http://moscova.inria.fr/%7Emaranget/papers/warn/warn.pdf
-- In the future i'll improve this implementation by mixing with
-- algorithm I

import Relude

import Nuko.Names          (ConsName, Name, Path, Qualified, TyName, ValName,
                            mkQualifiedPath)
import Nuko.Report.Range   (Range (..), emptyRange, getPos)
import Nuko.Report.Text    (Severity (..))
import Nuko.Tree           (Pat (PAnn, PCons, PId, PLit, POr, PRec, PWild),
                            RecordBinder (rbName, rbVal), Tc)
import Nuko.Tree.Expr      (Literal (..))
import Nuko.Typer.Env
import Nuko.Typer.Error    (TypeError (..))
import Nuko.Typer.Tree     ()
import Nuko.Typer.Types    (Relation (..), TTy (..))

import Control.Monad       (foldM)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet        qualified as HashSet

(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.) . (.)

-- I dont like the "boolean blindness" of maybe In this case
data ResSet
  = Infinite
  | Finite [(Qualified (Name ConsName), Int)]

data ConstructorSet
  = NotUsed ResSet
  | Complete [(Qualified (Name ConsName), Int)]

data Specialization
  = Constructor (Path (Name ConsName)) [Range -> Pat Tc]
  | Record [(Name ValName, FieldInfo)]
  | Literal (Literal Tc)

data Witness
  = IsUseful Bool -- Basically Algorithm 𝓤'

extend :: Witness -> Witness -> Witness
extend (IsUseful left) (IsUseful right) = IsUseful (left || right)

-- This function garantees an invariant... because this algorithm have a lot of this things
-- that should be garanteed by the type checker.
invariant :: Text -> Maybe a -> a
invariant t Nothing  = error $ "Compiler Error / Exhaustiveness Checking: " <> t
invariant _ (Just r) = r

-- Naive way to get the type
getTypeName :: TTy x -> Maybe (Qualified (Name TyName))
getTypeName = \case
  TyIdent e   -> pure e
  TyApp _ t _ -> getTypeName t
  _           -> Nothing

binderToWild :: [( (Name ValName), FieldInfo)] -> [Pat Tc]
binderToWild binders = PWild . (, emptyRange) . _fiType . snd <$> binders

completeFields :: [(Name ValName, FieldInfo)] -> [RecordBinder Pat Tc] -> [Pat Tc] -> [Pat Tc]
completeFields [] _ pats                       = reverse pats
completeFields ((name,field):rest) binders pats =
  case find (\binder -> binder.rbName == name) binders of
    Just binder -> completeFields rest binders (binder.rbVal:pats)
    Nothing     -> completeFields rest binders (PWild (field._fiType, emptyRange): pats)

-- Probably a bad way to do that but I cant think a better way without having to do some mess in another part.
specializeMatrix :: Specialization -> [[Pat Tc]] -> [[Pat Tc]]
specializeMatrix specialization matrix = do
    concatMap (go . (specialization, )) matrix
  where
    go = \case
      (Constructor path' _, PCons path pats _: rest) | path' == path -> [pats <> rest]
      (Constructor _ newPats, PId i _: rest) -> [flap newPats (getPos i)  <> rest]
      (Constructor _ newPats, PWild (_, i): rest) -> [flap newPats i <> rest]

      (Record allBinders, PRec _ binders _:rest) -> [completeFields allBinders binders [] <> rest]
      (Record allBinders, PId _ _: rest) -> [binderToWild allBinders <> rest]
      (Record allBinders, PWild _: rest) -> [binderToWild allBinders <> rest]

      (Literal (LStr t _), PLit (LStr t' _) _:rest) | t == t' -> [rest]
      (Literal (LInt t _), PLit (LInt t' _) _:rest) | t == t' -> [rest]
      (Literal _, PId _ _: rest) -> [rest]
      (Literal _, PWild _: rest) -> [rest]
      (Literal _, PLit _ _:_) -> []

      (spec, POr a b _:rest) -> specializeMatrix spec [a:rest, b:rest]
      (spec, PAnn pat _ _:rest) -> go (spec, pat:rest)
      (_, PCons {} : _) -> []
      (_, PLit {} : _) -> []
      (_, PRec {} : _) -> []
      (_, []) -> []

getUsedConstructors :: MonadTyper m => [[Pat Tc]] -> TTy x -> m ConstructorSet
getUsedConstructors matrix ty = do
    let tyName' = getTypeName ty
    case tyName' of
      Just name -> do
        (_, tyInfo) <- getTy tsTypes name
        case tyInfo._tyKind of
          IsTySyn       -> error "Not implemented type synonyms yet!"
          IsOpaque      -> pure (NotUsed Infinite)
          IsProdType {} -> pure (Complete [])
          IsSumType sum' -> do
            let consMap = HashMap.fromList $ toList sum'._stiConstructors
            if hasWild matrix
              then pure (Complete (HashMap.toList consMap))
              else do
                usedQualified <- traverse qualifyPath (concatMap getFirst matrix)
                let hashMap = HashSet.toMap $ HashSet.fromList usedQualified
                let notUsed = HashMap.difference consMap hashMap
                case HashMap.toList notUsed of
                  []       -> pure (Complete (HashMap.toList consMap))
                  (x : xs) -> pure $ NotUsed (Finite (x : xs))
      Nothing -> pure (NotUsed Infinite)
  where
    hasWild :: [[Pat Tc]] -> Bool
    hasWild []                 = False
    hasWild ((PWild _: _) : _) = True
    hasWild ((PId {}: _) : _)  = True
    hasWild (_ : rest)         = hasWild rest

    getFirst :: [Pat Tc] -> [Path (Name ConsName)]
    getFirst = \case
      (PCons name _ _:_) -> [name]
      (POr a b _: _)     -> getFirst [a] <> getFirst [b]
      (PAnn p _ _: _)    -> getFirst [p]
      _                  -> []

defaultMatrix :: [[Pat Tc]] -> [[Pat Tc]]
defaultMatrix = concatMap getRow where
  getRow = \case
    []                -> error "Checker Error: Should not have empty rows in default matrix"
    PCons {}   : _    -> []
    PLit {}    : _    -> []
    PRec {}    : _    -> []
    PWild _    : rest -> [rest]
    PId {}     : rest -> [rest]
    POr p q _  : rest -> defaultMatrix [p:rest, q:rest]
    PAnn p _ _ : rest -> getRow (p:rest)

getExhaustivenessWitness :: MonadTyper m => [[Pat Tc]] -> [Pat Tc] -> m Witness
getExhaustivenessWitness [] _ = pure (IsUseful True)
getExhaustivenessWitness _ [] = pure (IsUseful False)
getExhaustivenessWitness matrix (pat:rest) = case pat of
    POr left right _ -> do
      extend
        <$> getExhaustivenessWitness matrix (left:rest)
        <*> getExhaustivenessWitness matrix (right:rest)
    PCons path pats _ -> do
      qualified     <- qualifyPath path
      (_, dataInfo) <- getTy tsConstructors qualified
      let newPats   = dataInfo._tyTypes <&> curry PWild
      getExhaustivenessWitness
        (specializeMatrix (Constructor path newPats) matrix)
        (pats <> rest)
    PRec path binders _ -> do
      qualified <- qualifyPath path
      fields <- getTy tsTypeFields qualified
      let fields' = HashMap.toList fields
      let completed = completeFields fields' binders []
      getExhaustivenessWitness
        (specializeMatrix (Record fields') matrix)
        (completed <> rest)
    PLit lit _ -> do
      getExhaustivenessWitness
        (specializeMatrix (Literal lit) matrix)
        rest
    PWild ext    -> getExhaustivenessWild (snd ext) (fst ext)
    PId id' ext  -> getExhaustivenessWild (getPos id') ext
    PAnn p _ _   -> getExhaustivenessWitness matrix (p:rest)

  where
    -- Lol idk if haskell will make the traverse lazy or not.. so i have to implement my own function
    findFirstPositive :: MonadTyper m => Range -> [(Qualified (Name ConsName), Int)] -> m (Witness, Specialization)
    findFirstPositive _ []  = invariant "Specialization by qualifiers should not be empty" Nothing
    findFirstPositive range ((consName, _): rest') = do
      (_, info) <- getTy tsConstructors consName
      let wildcards = PWild .: (,) <$> info._tyTypes
      let specialization = Constructor (mkQualifiedPath consName) wildcards
      witness <- getExhaustivenessWitness (specializeMatrix specialization matrix) (flap wildcards range <> rest)
      case (witness, rest') of
        (_, [])            -> pure (witness, specialization)
        (IsUseful True, _) -> pure (witness, specialization)
        _                  -> findFirstPositive range rest'

    getExhaustivenessWild :: MonadTyper m => Range -> TTy x -> m Witness
    getExhaustivenessWild range typ = do
      constructorSet <- getUsedConstructors matrix typ
      case constructorSet of
        NotUsed _   -> getExhaustivenessWitness (defaultMatrix matrix) rest
        Complete [] -> do
          let tyName' = invariant "Not a record lol" $ getTypeName typ
          fields <- getTy tsTypeFields tyName'
          let fields' = HashMap.toList fields
          getExhaustivenessWitness
            (specializeMatrix (Record fields') matrix)
            ((PWild . (, emptyRange) . _fiType . snd <$> fields') <> rest)
        Complete (x:xs) -> fst <$> findFirstPositive range (x:xs)

checkPatterns :: MonadTyper m => Range -> TTy 'Real -> [Pat Tc] -> m ()
checkPatterns _ TyErr _               = pure ()
checkPatterns _ _ []                  = pure ()
checkPatterns r patternsType (x : xs) = do
    pats <- foldM (checkPat False) [[x]] xs
    _ <- checkPat True pats (PWild (patternsType, r))
    pure ()
  where
    checkPat :: MonadTyper m => Bool -> [[Pat Tc]] -> Pat Tc -> m [[Pat Tc]]
    checkPat isFinal matrix pat = do
      res <- getExhaustivenessWitness matrix [pat]
      case (res, isFinal) of
        (IsUseful True, True) -> emitDiagnostic Error (NotExhaustive (getPos pat) ("?" :| [])) (getPos pat)
        (IsUseful False, False) -> emitDiagnostic Warning (UselessClause (getPos pat)) (getPos pat)
        _ -> pure ()
      pure (matrix <> [[pat]])

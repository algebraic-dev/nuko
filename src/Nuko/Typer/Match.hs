module Nuko.Typer.Match (
  checkPatterns
) where

import Relude

import Nuko.Names          (ConsName, Name (..), Path, Qualified (..), TyName,
                            ValName, mkQualifiedPath)
import Nuko.Report.Range   (Range, emptyRange, getPos)
import Nuko.Tree           (Literal, Pat (..), Tc)
import Nuko.Tree.Expr      (Literal (..), RecordBinder (..))
import Nuko.Typer.Env      (DataConsInfo (..), FieldInfo (_fiResultType),
                            MonadTyper, SumTyInfo (..), TyInfo (..),
                            TyInfoKind (..), globalTypingEnv, qualifyPath,
                            tsConstructors, tsTypeFields, tsTypes)
import Nuko.Typer.Tree     ()
import Nuko.Typer.Types    (Relation (..), TTy (..))
import Pretty.Format       (Format (..))

import Control.Monad       (foldM)
import Data.HashMap.Strict qualified as HashMap
import Data.List           (lookup, nub)
import Lens.Micro.Platform (at, use)

data Witness = NoMatter Bool

type Matrix = [[Pat Tc]]

fieldToTuple :: RecordBinder Pat Tc -> (Name ValName, Pat Tc)
fieldToTuple (RecordBinder p pat _)  = (p, pat)

completeFields :: [(Name ValName, TTy 'Real)] -> [RecordBinder Pat Tc] -> [Pat Tc]  -> [Pat Tc]
completeFields []                _ acc       = reverse acc
completeFields ((name, ty) : xs) binders acc =
  completeFields xs binders (fromMaybe (PWild (ty, getPos name)) (lookup name (fieldToTuple <$> binders)) : acc)

-- Specializes a generic row into a matrix

specializeRow :: Int -> [Pat Tc] -> [[Pat Tc]]
specializeRow size = \case
  (PWild ext:rest)       -> [replicate size (PWild ext) <> rest]
  (PId ident ext:rest)   -> [replicate size (PWild (ext, getPos ident)) <> rest]
  (PLit _ _: _)          -> []
  (PRec _ _ _: _)        -> []
  (PCons _ _ _: _)       -> []
  (POr _ _ _ : _)        -> error "Compiler error: generic specialization should not have Or"
  (PAnn _ _ _ : _)       -> error "Compiler error: generic specialization should not have Ann"
  []                     -> error "Compiler Error: specializeRow should not have empty rows?"

specializeLiteral :: Literal Tc -> Matrix -> Matrix
specializeLiteral lit matrix = concatMap getRow matrix where
  getRow res = case (res, lit) of
    (PLit (LInt n _) _:rest, LInt n' _) | n == n' -> [rest] | otherwise -> []
    (PLit (LStr n _) _:rest, LStr n' _) | n == n' -> [rest] | otherwise -> []
    (POr p q _ : rest, _)            -> specializeLiteral lit [p:rest, q:rest]
    (PAnn pat _ _ : rest, _)         -> getRow (pat:rest)
    (pats, _)                        -> specializeRow 0 pats

specializeRecord :: Path (Name TyName) -> HashMap (Name ValName) FieldInfo -> Matrix -> Matrix
specializeRecord path names matrix = concatMap getRow matrix where
  getRow = \case
    (PRec path' binders _: rest)
      | path == path' -> [completeFields (second _fiResultType <$> HashMap.toList names) binders [] <> rest]
      | otherwise     -> []
    (POr p q _ : rest)     -> specializeRecord path names [p:rest, q:rest]
    (PAnn pat _ _ : rest)  -> getRow (pat:rest)
    pats -> specializeRow (length names) pats

specializeConstructor :: Path (Name ConsName) -> Int -> Matrix -> Matrix
specializeConstructor path size matrix = concatMap getRow matrix where
  getRow = \case
    (PCons path' pats _: rest)
      | path == path' -> [pats <> rest]
                               | otherwise     -> []
    (POr p q _ : rest)     -> specializeConstructor path size [p:rest, q:rest]
    (PAnn pat _ _ : rest)  -> getRow (pat:rest)
    pats -> specializeRow size pats

-- WItness

mkUseless :: Witness
mkUseless = NoMatter False

mkUseful :: Witness
mkUseful = NoMatter True

extend :: Witness -> Witness -> Witness
extend (NoMatter a) (NoMatter b) = NoMatter (a || b)

isUsefulWitness :: Witness -> Bool
isUsefulWitness (NoMatter t) = t

-- Functions to get data

ensure :: Maybe a -> a
ensure (Just n) = n
ensure Nothing  = error "Compiler error: Constructor does not exists!"

getConsInfo  :: MonadTyper m => Path (Name ConsName) -> m DataConsInfo
getConsInfo name = do
  qualPath <- qualifyPath name
  snd . ensure <$> use (globalTypingEnv . tsConstructors . at qualPath)

getRecordInfo  :: MonadTyper m => Path (Name TyName) -> m (HashMap (Name ValName) FieldInfo)
getRecordInfo name = do
  qualPath <- qualifyPath name
  tyInfo   <- use (globalTypingEnv . tsTypeFields . at qualPath)
  pure (fromMaybe (error "Compiler Error: Not well typed") tyInfo)

getPatTy :: Pat Tc -> TTy 'Real
getPatTy = \case
  PWild ty     -> fst ty
  PId _ ty     -> ty
  POr _ _ ty   -> fst ty
  PCons _ _ ty -> fst ty
  PLit _ ty    -> ty
  PAnn _ _ ty  -> fst ty
  PRec _ _ ty  -> fst ty

-- Usefulness

getTy :: TTy 'Real -> Maybe (Qualified (Name TyName))
getTy = \case
  TyIdent e   -> pure e
  TyApp _ t _ -> getTy t
  _           -> Nothing

getConstTyFields :: MonadTyper m => Qualified (Name TyName) -> m TyInfo
getConstTyFields tyName = snd . ensure <$> use (globalTypingEnv . tsTypes . at tyName)

getUsedConstructors :: Matrix -> [Path (Name ConsName)]
getUsedConstructors = nub . concatMap getRow where
  getRow :: [Pat Tc] -> [Path (Name ConsName)]
  getRow = \case
    (PCons name _ _:_) -> [name]
    _                  -> []

defaultMatrix :: Matrix -> Matrix
defaultMatrix s = concatMap getRow s where
  getRow = \case
    []                -> error "Checker Error: Should not have empty rows in default matrix"
    PCons {}   : _    -> []
    PLit {}    : _    -> []
    PRec {}    : _    -> []
    PWild _    : rest -> [rest]
    PId {}     : rest -> [rest]
    POr p q _  : rest -> defaultMatrix [p:rest, q:rest]
    PAnn p _ _ : rest -> getRow (p:rest)



isIdentifierUseful :: MonadTyper m => [TTy 'Real] -> Matrix -> [Pat Tc] -> TTy 'Real -> Range -> m Witness
isIdentifierUseful currentTy matrix rest ty range =
    case getTy ty of
      Just name -> do
        typeInfo <- getConstTyFields name
        case typeInfo._tyKind of
          IsSumType  info -> sumTypeWitness info
          IsProdType info -> error "Not supported yet?"
          IsBuiltIn       -> error "Not supported yet?"
          _               -> error "Compiler Error: Not supported lol"
      Nothing -> do
        isUseful currentTy (defaultMatrix matrix) rest
  where
    findFirstNotUseful []              = error "Should not be empty wtf"
    findFirstNotUseful [const']        = (const', ) <$> specializeWithCurrentParams const'
    findFirstNotUseful (const': rest') = do
      witness <- specializeWithCurrentParams const'
      if isUsefulWitness witness
        then findFirstNotUseful rest'
        else pure (const', witness)

    specializeWithCurrentParams ((consName, size), tty) =
      isUseful (tty <> currentTy)
        (specializeConstructor (mkQualifiedPath consName) size matrix)
        (((PWild . (, range)) <$> tty) <> rest)

    sumTypeWitness info = do
      let usedConstructors = getUsedConstructors matrix
      if length info._stiConstructors == length usedConstructors
        then do
          let constructors = zip (toList info._stiConstructors) info._stiTypes
          (_, witness)    <- findFirstNotUseful constructors
          pure witness
        else isUseful currentTy (defaultMatrix matrix) rest

isUseful :: MonadTyper m => [TTy 'Real] -> Matrix -> [Pat Tc] -> m Witness
isUseful _ [] []                       = pure mkUseful
isUseful _ matrix [] | all null matrix = pure mkUseless
isUseful _ _ []                        = error "Compiler Error: Probably the pattern matching entry is not well typed!"
isUseful (fstTy : restTy) matrix (pat : rest) = case pat of
  PWild ty    -> isIdUseful (fst ty) (snd ty)
  PId i ty    -> isIdUseful ty (getPos i)
  POr p q _   -> extend <$> isUsefulDef (p:rest) <*> isUsefulDef (q:rest)
  PCons n p _ -> do
    info <- getConsInfo n
    isUseful ((getPatTy <$> p) <> restTy) (specializeConstructor n info._parameters matrix) (p <> rest)
  PLit lit _    -> do
    isUseful (restTy) (specializeLiteral lit matrix) rest
  PAnn pat' _ _ ->
    isUseful (fstTy : restTy) matrix (pat' : rest)
  PRec path binders _  -> do
    recordInfo <- getRecordInfo path
    let pats    = completeFields (HashMap.toList (_fiResultType <$> recordInfo)) binders []
    isUseful [] (specializeRecord path recordInfo matrix) (pats <> rest)
  where
    isUsefulDef = isUseful (restTy) matrix
    isIdUseful  = isIdentifierUseful restTy matrix rest
isUseful _ _ _ = error "Compiler Error: Probably the pattern matching entry is not well typed!!"

checkPatterns :: MonadTyper m => TTy 'Real -> [Pat Tc] -> m ()
checkPatterns TyErr _               = pure ()
checkPatterns patternsType (x : xs) = do
    pats <- foldM checkPat [[x]] xs
    _ <- checkPat pats (PWild (patternsType, emptyRange))
    pure ()
  where
    checkPat :: MonadTyper m => Matrix -> Pat Tc -> m Matrix
    checkPat matrix pat = do
      res <- isUseful [patternsType] matrix [pat]
      case res of NoMatter res' -> putTextLn ("<Useful " <> show res' <> ">")
      pure (matrix <> [[pat]])

checkPatterns _ []        = pure ()

module Nuko.Typer.Match (
  checkPatterns
) where

import Relude

import Nuko.Names               (ConsName, Name (..), Path, Qualified (..),
                                 TyName, ValName, mkQualifiedPath)
import Nuko.Report.Range        (Range, emptyRange, getPos)
import Nuko.Tree                (Literal, Pat (..), Tc)
import Nuko.Tree.Expr           (Literal (..), RecordBinder (..))
import Nuko.Typer.Env           (DataConsInfo (..), FieldInfo (_fiResultType),
                                 MonadTyper, SumTyInfo (..), TyInfo (..),
                                 TyInfoKind (..), globalTypingEnv, qualifyPath,
                                 tsConstructors, tsTypeFields, tsTypes)
import Nuko.Typer.Infer.Literal (preludeQual)
import Nuko.Typer.Tree          ()
import Nuko.Typer.Types         (Relation (..), TTy (..))
import Pretty.Format            (Format (..))

import Control.Monad            (foldM)
import Data.HashMap.Strict      qualified as HashMap
import Data.List                (lookup, nub)
import Lens.Micro.Platform      (at, use)

data Witness
  = NoMatter Bool
  | Witness  (Maybe [Pat Tc])

type Matrix = [[Pat Tc]]

-- Specialization

specializeRow :: Int -> [Pat Tc] -> [[Pat Tc]]
specializeRow size = \case
  (PWild ext:rest)     -> [replicate size (PWild ext) <> rest]
  (PId ident ext:rest) -> [replicate size (PWild (ext, getPos ident)) <> rest]
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

fieldToTuple :: RecordBinder Pat Tc -> (Name ValName, Pat Tc)
fieldToTuple (Mono p ty)     = (p, PId p ty)
fieldToTuple (Binder p ty _) = (p, ty)

completeFields :: [(Name ValName, TTy 'Real)] -> [(Name ValName, Pat Tc)] -> [Pat Tc]  -> [Pat Tc]
completeFields []                _ acc       = reverse acc
completeFields ((name, ty) : xs) binders acc =
  completeFields xs binders (fromMaybe (PWild (ty, getPos name)) (lookup name binders) : acc)

specializeRecord :: Path (Name TyName) -> HashMap (Name ValName) FieldInfo -> Matrix -> Matrix
specializeRecord path names matrix = concatMap getRow matrix where
  getRow = \case
    (PRec path' binders _: rest)
      | path == path' -> [completeFields (second _fiResultType <$> HashMap.toList names) (fieldToTuple <$> binders) [] <> rest]
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

mkUseless :: Bool -> Witness
mkUseless True  = Witness Nothing
mkUseless False = NoMatter False

mkUseful :: Bool -> Witness
mkUseful True  = Witness (Just [])
mkUseful False = NoMatter True

extend :: Witness -> Witness -> Witness
extend (NoMatter True) _         = NoMatter True
extend (NoMatter a) (NoMatter b) = NoMatter (a || b)
extend (Witness a) (Witness b)   = Witness ((<>) <$> a <*> b)
extend _ _                       = error "Compiler Error: like.. extending a witness this way should not be possible?"

isUsefulWitness :: Witness -> Bool
isUsefulWitness = \case
  (Witness (Just _)) -> True
  (NoMatter t)       -> t
  _                  -> False

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

-- The name of this functions lies a little bit. It does not only gets
-- the type, it just not relies on the type if it's a LIteral type because..
-- you know, they cannot be satisfied
getTy :: TTy 'Real -> Maybe (Qualified (Name TyName))
getTy = \case
  TyIdent e
    | e == preludeQual "Int" -> Nothing
    | e == preludeQual "String" -> Nothing
    | otherwise -> pure e
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
    PCons {}   :_     -> []
    PLit {}    :_     -> []
    PRec {}    :_     -> []
    PWild _    :rest  -> [rest]
    PId {}     :rest  -> [rest]
    POr p q _  :rest  -> defaultMatrix [p:rest, q:rest]
    PAnn p _ _ : rest -> getRow (p:rest)

isIdentifierUseful :: MonadTyper m => Bool -> [TTy 'Real] -> Matrix -> [Pat Tc] -> TTy 'Real -> Range -> m Witness
isIdentifierUseful returnWitness currentTy matrix rest ty range =
    case getTy ty of
      Just name -> do
        typeInfo <- getConstTyFields name
        case typeInfo._tyKind of
          IsSumType info -> do
            let usedConstructors = getUsedConstructors matrix
            if length info._stiConstructors == length usedConstructors
              then completeWitness info typeInfo._resultantType
              else incompleteWitness usedConstructors info
          IsProdType info -> undefined
          IsBuiltIn       -> error "Not supported yet?"
          _               -> error "Compiler Error: Not supported lol"
      Nothing -> do
        isUseful returnWitness currentTy (defaultMatrix matrix) rest
  where
    findFirstNotUseful []              = error "Should not be empty wtf"
    findFirstNotUseful [const']        = (const', ) <$> usefulSpecialized const'
    findFirstNotUseful (const': rest') = do
      witness <- usefulSpecialized const'
      if isUsefulWitness witness
        then findFirstNotUseful rest'
        else pure (const', witness)

    usefulSpecialized ((consName, size), tty) =
      isUseful returnWitness (tty <> currentTy)
        (specializeConstructor (mkQualifiedPath consName) size matrix)
        (((\ty' -> PWild (ty', range)) <$> tty) <> rest)

    completeWitness info resTy = do
      let constructors  = zip (toList info._stiConstructors) info._stiTypes
      (((name, size), _), witness) <- findFirstNotUseful constructors
      case witness of
        Witness (Just pats) -> do
          let (args, ret) = splitAt size pats
          pure $ Witness (Just (PCons (mkQualifiedPath name) args (resTy, emptyRange) : ret))
        _ -> pure witness

    incompleteWitness used allCons = do
      witness <- isUseful returnWitness currentTy (defaultMatrix matrix) rest
      case (witness, used) of
        (Witness (Just res), []) -> pure (Witness (Just (PWild (TyErr, emptyRange) : res)))
        (Witness (Just res), o)  -> undefined
        _ -> pure witness

isUseful :: MonadTyper m => Bool -> [TTy 'Real] -> Matrix -> [Pat Tc] -> m Witness
isUseful cond _ [] []                       = pure $ mkUseful cond
isUseful cond _ matrix [] | all null matrix = pure $ mkUseless cond
isUseful _    _  m []                       = error $ "Compiler Error: Probably the pattern matching entry is not well typed! |" <> (show (length <$> m) <> "|")
isUseful returnWitness (fstTy : restTy) matrix (pat : rest) = case pat of
  PWild ty    -> isIdUseful (fst ty) (snd ty)
  PId i ty    -> isIdUseful ty (getPos i)
  POr p q _   -> extend <$> isUsefulDef (p:rest) <*> isUsefulDef (q:rest)
  PCons n p _ -> do
    info <- getConsInfo n
    isUseful returnWitness ((getPatTy <$> p) <> restTy) (specializeConstructor n info._parameters matrix) (p <> rest)
  PLit lit _    -> do
    isUseful returnWitness (restTy) (specializeLiteral lit matrix) rest
  PAnn pat' _ _ ->
    isUseful returnWitness (fstTy : restTy) matrix (pat' : rest)
  PRec path binders _  -> do
    let tuples = fieldToTuple <$> binders
    recordInfo <- getRecordInfo path
    let pats   = completeFields (HashMap.toList (_fiResultType <$> recordInfo)) tuples []
    isUseful returnWitness [] (specializeRecord path recordInfo matrix) (pats <> rest)
  where
    isUsefulDef = isUseful returnWitness (restTy) matrix
    isIdUseful  = isIdentifierUseful returnWitness restTy matrix rest
isUseful _    _  m _ = do
  error $ "Compiler Error: Probably the pattern matching entry is not well typed!! |" <> format m <> "|"

checkPatterns :: MonadTyper m => TTy 'Real -> [Pat Tc] -> m ()
checkPatterns TyErr _               = pure ()
checkPatterns patternsType (x : xs) = do
    pats <- foldM checkPat [[x]] xs
    checkPat pats (PWild (patternsType, emptyRange))
    pure ()
  where
    checkPat :: MonadTyper m => Matrix -> Pat Tc -> m Matrix
    checkPat matrix pat = do
      res <- isUseful False [patternsType] matrix [pat]
      case res of
        NoMatter res' -> putTextLn ("<Useful " <> show res' <> ">")
        _             -> putTextLn "????"
      pure (matrix <> [[pat]])

checkPatterns _ []        = pure ()

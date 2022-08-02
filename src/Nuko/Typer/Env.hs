module Nuko.Typer.Env (
  DataConsInfo(..),
  FieldInfo(..),
  TypeSpace(..),
  DefInfo(..),
  TyInfo(..),
  TyInfoKind(..),
  TypingEnv(..),
  SumTyInfo(..),
  ProdTyInfo(..),
  MonadTyper,
  teTrackers,
  qualifyTyName,
  endDiagnostic,
  emitDiagnostic,
  emptyTypeSpace,
  addFieldToEnv,
  getKind,
  addTy,
  qualifyLocal,
  updateTyKind,
  addTyKind,
  qualifyPath,
  eagerInstantiate,
  newKindHole,
  newTyHole,
  genTyHole,
  getLocal,
  addLocals,
  parameters,
  fiResultType,
  tsConstructors,
  tsTypes,
  tyName,
  globalTypingEnv,
  tsVars,
  seScope,
  seTyEnv,
  addLocalTy,
  getKindMaybe,
  getTy,
  runInst,
  newTyHoleWithScope,
  tsTypeFields,
  runToIO,
  genKindHole,
  addLocalTypes,
) where

import Relude

import Nuko.Names
import Nuko.Report.Message           (Diagnostic (..), DiagnosticInfo (..),
                                      Severity (..))
import Nuko.Report.Range             (Range, getPos)
import Nuko.Resolver.Tree            ()
import Nuko.Typer.Error              (TypeError (NameResolution))
import Nuko.Typer.Error.Tracking     (Tracker)
import Nuko.Typer.Types              (Hole (..), Relation (..), TKind (..),
                                      TTy (..), derefTy)
import Nuko.Utils                    (flag, terminate)

import Control.Monad.Chronicle       (MonadChronicle)
import Data.These                    (These)
import Lens.Micro.Platform           (Lens', at, makeLenses, over, use, (%=))
import Pretty.Tree                   (PrettyTree)

import Control.Monad.Reader          qualified as Reader
import Control.Monad.State.Strict    qualified as State
import Control.Monad.Trans.Chronicle qualified as Chronicle
import Data.HashMap.Strict           qualified as HashMap

newtype SumTyInfo = SumTyInfo
  { _stiConstructors :: NonEmpty (Qualified (Name ConsName), Int)
  } deriving Generic

newtype ProdTyInfo = ProdTyInfo
  { _ptiFields :: [Name ValName]
  } deriving Generic

data TyInfoKind
  = IsTySyn
  | IsSumType SumTyInfo
  | IsProdType ProdTyInfo
  | IsBuiltIn -- TODO: Remove this in the future?
  deriving Generic

data TyInfo = TyInfo
  { _resultantType :: TTy 'Real
  , _label         :: Name TyName
  , _tyNames       :: [(Name TyName, TKind)]
  , _tyKind        :: TyInfoKind
  } deriving Generic

data DefInfo = DefInfo
  { _diResultType :: TTy 'Real
  , _polymorphics :: [(Name TyName, TKind)]
  , _argTypes     :: [TTy 'Real]
  , _retType      :: TTy 'Real
  } deriving Generic

newtype FieldInfo = FieldInfo
  { _fiResultType :: TTy 'Real
  } deriving Generic

data DataConsInfo = DataConsInfo
  { _parameters :: Int
  , _tyName     :: Qualified (Name TyName)
  } deriving Generic

data TypeSpace = TypeSpace
  { _tsTypes        :: HashMap (Qualified (Name TyName)) (TKind, TyInfo)
  , _tsConstructors :: HashMap (Qualified (Name ConsName)) (TTy 'Real, DataConsInfo)
  , _tsVars         :: HashMap (Qualified (Name ValName)) (TTy 'Real, DefInfo)
  , _tsTypeFields   :: HashMap (Qualified (Name TyName)) (HashMap (Name ValName) FieldInfo)
  } deriving Generic

data TypingEnv = TypingEnv
  { _teCurModule     :: ModName
  , _teCurFilename   :: Text
  , _teTrackers      :: [Tracker]
  , _globalTypingEnv :: TypeSpace
  } deriving Generic

data ScopeEnv = ScopeEnv
  { _seVars  :: HashMap (Name ValName) (TTy 'Virtual)
  , _seTyEnv :: [(Name TyName, TKind)]
  , _seScope :: Int
  } deriving Generic

makeLenses ''DataConsInfo
makeLenses ''FieldInfo
makeLenses ''TypeSpace
makeLenses ''TypingEnv
makeLenses ''ScopeEnv

instance PrettyTree TypingEnv where
instance PrettyTree FieldInfo where
instance PrettyTree SumTyInfo where
instance PrettyTree ProdTyInfo where
instance PrettyTree TyInfoKind where
instance PrettyTree DataConsInfo where
instance PrettyTree DefInfo where
instance PrettyTree TypeSpace where
instance PrettyTree TyInfo where

type MonadTyper m =
  ( MonadIO m
  , MonadState TypingEnv m
  , MonadReader ScopeEnv m
  , MonadChronicle (Endo [Diagnostic]) m
  )

endDiagnostic :: MonadTyper m => TypeError -> Range -> m b
endDiagnostic kind' range = do
  modName <- use teCurModule
  fileName <- use teCurFilename
  terminate $ Diagnostic
    { moduleName = modName
    , severity = Error
    , filename = fileName
    , position = range
    , kind = TypingError kind'
    }

emitDiagnostic :: MonadTyper m => Severity -> TypeError -> Range -> m ()
emitDiagnostic severity' kind range = do
  modName <- use teCurModule
  fileName <- use teCurFilename
  flag $ Diagnostic
    { moduleName = modName
    , severity = severity'
    , filename = fileName
    , position = range
    , kind = TypingError kind
    }

emptyScopeEnv :: ScopeEnv
emptyScopeEnv = ScopeEnv HashMap.empty [] 0

emptyTypeSpace :: TypeSpace
emptyTypeSpace = TypeSpace HashMap.empty HashMap.empty HashMap.empty HashMap.empty

runToIO :: TypeSpace -> ModName -> Text -> (forall m. MonadTyper m => m a) -> IO (These [Diagnostic] (a, TypingEnv))
runToIO typeSpace modName filename action = do
  let env = TypingEnv modName filename [] typeSpace
  let res = Reader.runReaderT action emptyScopeEnv
  first (`appEndo` [])
    <$> Chronicle.runChronicleT (State.runStateT res env)

runInst :: MonadTyper m => m (b, TTy 'Virtual) -> m (b, TTy 'Virtual)
runInst fun = do
  (resE, resTy) <- fun
  instTy <- eagerInstantiate resTy
  pure (resE, derefTy instTy)

addLocals :: MonadTyper m => HashMap (Name ValName) (TTy 'Virtual) -> m a -> m a
addLocals newLocals = local (over seVars (<> newLocals))

getLocal :: MonadTyper m => Name ValName -> m (TTy 'Virtual)
getLocal name = do
  maybeRes <- asks (HashMap.lookup name . _seVars)
  case maybeRes of
    Just res -> pure res
    Nothing  -> endDiagnostic (NameResolution (getPos name) (Label name)) (getPos name)

newTyHole :: MonadTyper m => Name TyName -> m (TTy k)
newTyHole name = asks _seScope >>= newTyHoleWithScope name

newTyHoleWithScope :: MonadTyper m => Name TyName -> Int -> m (TTy k)
newTyHoleWithScope name scope = TyHole <$> newIORef (Empty name scope)

genTyHole :: MonadTyper m => m (TTy 'Virtual)
genTyHole = newTyHole (mkName TyName (genIdent "_") Untouched)

newKindHole :: MonadTyper m => Name TyName -> m TKind
newKindHole name = do
  curScope <- asks _seScope
  KiHole <$> newIORef (Empty name curScope)

genKindHole :: MonadTyper m => m TKind
genKindHole = newKindHole (mkName TyName (genIdent "_") Untouched)

eagerInstantiate :: MonadTyper m => TTy 'Virtual -> m (TTy 'Virtual)
eagerInstantiate = \case
  TyForall name f -> do
    hole <- newTyHole name
    eagerInstantiate (f hole)
  TyHole hole -> do
    content <- readIORef hole
    case content of
      Filled f -> eagerInstantiate f
      Empty {} -> pure $ TyHole hole
  other -> pure other

qualifyLocal :: MonadTyper m => Name k -> m (Qualified (Name k))
qualifyLocal name = do
  modName <- use teCurModule
  pure (mkQualified modName name (getPos name))

qualifyPath :: MonadTyper m => Path (Name k) -> m (Qualified (Name k))
qualifyPath (Local _ name)     = qualifyLocal name
qualifyPath (Full _ qualified) = pure qualified

addTyKind :: MonadTyper m => Name TyName -> TKind -> TyInfo -> m ()
addTyKind name ki info = qualifyLocal name >>= \name' -> globalTypingEnv . tsTypes %= HashMap.insert name' (ki, info)

updateTyKind :: MonadTyper m => Qualified (Name TyName) -> ((TKind, TyInfo) -> Maybe (TKind, TyInfo)) -> m ()
updateTyKind name f = globalTypingEnv . tsTypes %= HashMap.update f name

qualifyTyName ::MonadTyper m =>  Maybe Ident -> Name k -> m (Qualified (Name k))
qualifyTyName tyName' name = do
  qual@(Qualified _ (ModName _ e _) ident r') <- qualifyLocal name
  pure $ maybe qual (\tyName'' -> mkQualified (mkModName (e <> pure tyName'')) ident r') tyName'

addTy :: MonadTyper m => Lens' TypeSpace (HashMap (Qualified (Name k)) b) -> Maybe Ident -> Name k -> b -> m ()
addTy lens tyName' name ki = do
  name' <- qualifyTyName tyName' name
  globalTypingEnv . lens %= HashMap.insert name' ki

getTy :: MonadTyper m => Lens' TypeSpace (HashMap (Qualified (Name k)) b) -> Qualified (Name k) -> m b
getTy lens qualified = do
  result <- use (globalTypingEnv . lens . at qualified)
  case result of
    Just res -> pure res
    Nothing  -> endDiagnostic (NameResolution (getPos qualified.qInfo) (Label $ qualified.qInfo)) (getPos qualified.qInfo)

getKindMaybe :: MonadTyper m => Path (Name TyName) -> m (Maybe TKind)
getKindMaybe path = do
  canon  <- qualifyPath path
  result <- use (globalTypingEnv . tsTypes . at canon)
  pure (fst <$> result)

getKind :: MonadTyper m => Path (Name TyName) -> m TKind
getKind path = do
  result <- getKindMaybe path
  maybe (endDiagnostic (NameResolution (getPos path) (Label $ getPathInfo path)) (getPos path)) pure result

addFieldToEnv :: MonadTyper m => Name TyName -> Name ValName -> FieldInfo -> m ()
addFieldToEnv typeName fieldName fieldInfo = do
  qualified <- qualifyLocal typeName
  globalTypingEnv . tsTypeFields %= HashMap.insertWith (<>) qualified (HashMap.singleton fieldName fieldInfo)

addLocalTy :: MonadTyper m => Name TyName -> TKind -> m a -> m a
addLocalTy name ty = local (over seTyEnv ((name, ty) :) . over seScope (+ 1))

addLocalTypes :: MonadTyper m => [(Name TyName, TKind)] -> m a -> m a
addLocalTypes names = local (over seTyEnv (names <>))

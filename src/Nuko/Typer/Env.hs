module Nuko.Typer.Env (
  DataConsInfo(..),
  FieldInfo(..),
  TypeSpace(..),
  addFieldToEnv,
  getKind,
  addTy,
  updateTyKind, 
  addTyKind,
  qualifyPath,
  eagerInstantiate,
  newKindHole,
  newTyHole,
  getLocal,
  addLocals,
  constructorTy,
  parameters,
  fiResultType,
  tsConstructors,
  tsVars,
  seScope
) where

import Relude                  (Generic, Int, MonadIO, pure, (.), (<$>), ($), Monad ((>>=)), fst)
import Relude.Monad            (MonadState, MonadReader (local), Maybe (..), asks, maybe)
import Relude.Monoid           (Endo, (<>))
import Relude.Lifted           (newIORef)

import Nuko.Names              (Name, ConsName, TyName, ValName, ModName (..), Label (..), Path (..), mkQualified, Qualified, getPathInfo)
import Nuko.Utils              (terminate)
import Nuko.Typer.Types        (TTy (..), Relation (..), TKind)
import Nuko.Typer.Types        (Hole (..), TKind (..))
import Nuko.Typer.Error        (TypeError(NameResolution))
import Nuko.Report.Range       (getPos)
import Nuko.Resolver.Tree      ()

import Pretty.Tree             (PrettyTree)
import Data.HashMap.Strict     (HashMap)
import Lens.Micro.Platform     (makeLenses, over, Lens', use, (%=), at)
import Control.Monad.Chronicle (MonadChronicle)
import qualified Data.HashMap.Strict as HashMap

data TyInfo
  = IsTySyn
  | IsTyDef
  deriving Generic

data FieldInfo = FieldInfo
  { _fiResultType :: TTy 'Virtual
  } deriving Generic

data DataConsInfo = DataConsInfo
  { _constructorTy :: TTy 'Virtual
  , _parameters    :: Int
  } deriving Generic

data TypeSpace = TypeSpace
  { _tsTypes        :: HashMap (Qualified (Name TyName)) (TKind, TyInfo)
  , _tsConstructors :: HashMap (Qualified (Name ConsName)) DataConsInfo
  , _tsVars         :: HashMap (Qualified (Name ValName)) (TTy 'Virtual)
  , _tsTypeFields   :: HashMap (Qualified (Name TyName)) (HashMap (Name ValName) FieldInfo)
  } deriving Generic

data TypingEnv = TypingEnv
  { _teCurModule     :: ModName
  , _globalTypingEnv :: TypeSpace
  } deriving Generic

data ScopeEnv = ScopeEnv
  { _seVars  :: HashMap (Name ValName) (TTy 'Virtual)
  , _seScope :: Int
  } deriving Generic


makeLenses ''DataConsInfo
makeLenses ''FieldInfo
makeLenses ''TypeSpace
makeLenses ''TypingEnv
makeLenses ''ScopeEnv

instance PrettyTree TypingEnv where
instance PrettyTree FieldInfo where
instance PrettyTree TyInfo where
instance PrettyTree DataConsInfo where
instance PrettyTree TypeSpace where

type MonadTyper m =
  ( MonadIO m
  , MonadState TypingEnv m
  , MonadReader ScopeEnv m
  , MonadChronicle (Endo [TypeError]) m
  )

addLocals :: MonadTyper m => HashMap (Name ValName) (TTy 'Virtual) -> m a -> m a
addLocals newLocals = local (over seVars (<> newLocals))

getLocal :: MonadTyper m => Name ValName -> m (TTy 'Virtual)
getLocal name = do
  maybeRes <- asks (HashMap.lookup name . _seVars)
  case maybeRes of
    Just res -> pure res
    Nothing  -> terminate (NameResolution $ Label $ name)

newTyHole :: MonadTyper m => Name TyName -> m (TTy 'Virtual)
newTyHole name = do
  curScope <- asks _seScope
  TyHole <$> newIORef (Empty name curScope)

newKindHole :: MonadTyper m => Name TyName -> m TKind
newKindHole name = do
  curScope <- asks _seScope
  KiHole <$> newIORef (Empty name curScope)

eagerInstantiate :: MonadTyper m => TTy 'Virtual -> m (TTy 'Virtual)
eagerInstantiate = \case
  TyForall name f -> do
    hole <- newTyHole name
    eagerInstantiate (f hole)
  other -> pure other

qualifyLocal :: MonadTyper m => Name k -> m (Qualified (Name k))
qualifyLocal name = do
  modName <- use teCurModule
  pure (mkQualified modName name (getPos modName))

qualifyPath :: MonadTyper m => Path (Name k) -> m (Qualified (Name k))
qualifyPath (Local _ name) = qualifyLocal name
qualifyPath (Full _ qualified) = pure qualified

addTyKind :: MonadTyper m => Name TyName -> TKind -> TyInfo -> m ()
addTyKind name ki info = qualifyLocal name >>= \name' -> globalTypingEnv . tsTypes %= HashMap.insert name' (ki, info)

updateTyKind :: MonadTyper m => Qualified (Name TyName) -> ((TKind, TyInfo) -> Maybe (TKind, TyInfo)) -> m ()
updateTyKind name f = globalTypingEnv . tsTypes %= HashMap.update f name

addTy :: MonadTyper m => Lens' TypeSpace (HashMap (Qualified (Name k)) b) -> Name k -> b -> m ()
addTy lens name ki = qualifyLocal name >>= \name' -> globalTypingEnv . lens %= HashMap.insert name' ki

getKind :: MonadTyper m => Path (Name TyName) -> m TKind
getKind path = do
  canon  <- qualifyPath path
  result <- use (globalTypingEnv . tsTypes . at canon)
  maybe (terminate (NameResolution (Label $ getPathInfo path))) (pure . fst) result

addFieldToEnv :: MonadTyper m => (Name TyName) -> Name ValName -> FieldInfo -> m ()
addFieldToEnv typeName fieldName fieldInfo = do
  qualified <- qualifyLocal typeName
  globalTypingEnv . tsTypeFields %= HashMap.insertWith ((<>)) qualified (HashMap.singleton fieldName fieldInfo)
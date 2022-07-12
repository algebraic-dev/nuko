module Nuko.Typer.Env (
  TypeSpace(..),
  FieldInfo(..),
  TypingEnv(..),
  TyInfo(..),
  ScopeEnv(..),
  MonadTyper,
  getKind,
  tsTypes,
  addTyKind,
  unifyHolesWithStar,
  tsConstructors,
  addTy,
  emptyTS,
  tsTypeFields,
  fiResultType,
  addFieldToEnv,
  teCurModule,
  updateTyKind,
  seScope,
  seVars,
  eagerInstantiate,
  removeHoles,
  newTyHole,
  getTy,
  addLocals,
  getLocal,
  newKindHole,
  tsVars
) where

import Relude.Applicative      (Applicative(pure, (<*>), (*>)))
import Relude.Function         ((.), ($))
import Relude.Functor          ((<$>))
import Relude.Monad            (MonadIO, Maybe (..), MonadState, Either (..), either, modify, gets, MonadReader(local), asks)
import Relude.String           (Text)
import Relude.Monoid           ((<>), Endo)
import Relude.Lifted           (readIORef, writeIORef, newIORef)
import Relude.Base             (Generic)

import Nuko.Utils              (terminate)
import Nuko.Typer.Types        (Hole (..), TKind (..), TTy (..), Virtual, Int)
import Nuko.Typer.Error        (TypeError(NameResolution))
import Nuko.Resolver.Tree      (Path (..), ReId(text))

import Pretty.Tree             (PrettyTree)
import Data.HashMap.Strict     (HashMap, lookup)
import Control.Monad.Chronicle (MonadChronicle)
import Lens.Micro.Platform     (makeLenses, over, Lens', view)

import qualified Data.HashMap.Strict as HashMap
import Relude (trace, show)

data TyInfo
  = IsTySyn
  | IsTyDef
  deriving Generic

data FieldInfo = FieldInfo
  { _fiResultType :: TTy Virtual
  } deriving Generic

data TypeSpace = TypeSpace
  { _tsTypes        :: HashMap Text (TKind, TyInfo)
  , _tsConstructors :: HashMap Text (TTy Virtual, Int)
  , _tsVars         :: HashMap Text (TTy Virtual)
  , _tsTypeFields   :: HashMap Text (HashMap Text FieldInfo)
  } deriving Generic

data TypingEnv = TypingEnv
  { _teCurModule     :: Text
  , _globalTypingEnv :: TypeSpace
  } deriving Generic

data ScopeEnv = ScopeEnv
  { _seVars  :: HashMap Text (TTy Virtual)
  , _seScope :: Int
  } deriving Generic

makeLenses ''FieldInfo
makeLenses ''TypeSpace
makeLenses ''TypingEnv
makeLenses ''ScopeEnv

instance PrettyTree TypingEnv where
instance PrettyTree FieldInfo where
instance PrettyTree TyInfo where
instance PrettyTree TypeSpace where

type MonadTyper m =
  ( MonadIO m
  , MonadState TypingEnv m
  , MonadReader ScopeEnv m
  , MonadChronicle (Endo [TypeError]) m
  )

addLocals :: MonadTyper m => HashMap Text (TTy Virtual) -> m a -> m a
addLocals newLocals = local (over seVars (<> newLocals))

getLocal :: MonadTyper m => Text -> m (TTy Virtual)
getLocal name = do
  maybeRes <- asks (HashMap.lookup name . _seVars)
  case maybeRes of
    Just res -> pure res
    Nothing  -> terminate (NameResolution name)

newTyHole :: MonadTyper m => Text -> m (TTy Virtual)
newTyHole name = do
  curScope <- asks _seScope
  TyHole <$> newIORef (Empty name curScope)

newKindHole :: MonadTyper m => Text -> m TKind
newKindHole name = do
  curScope <- asks _seScope
  KiHole <$> newIORef (Empty name curScope)

eagerInstantiate :: MonadTyper m => TTy Virtual -> m (TTy Virtual)
eagerInstantiate = \case
  TyForall name f -> do
    hole <- newTyHole name
    eagerInstantiate (f hole)
  other -> pure other

removeHoles :: MonadTyper m => TTy Virtual -> m (TTy Virtual)
removeHoles = \case
    TyHole hole -> do
      content <- readIORef hole
      case content of
        Empty {} -> pure (TyHole hole)
        Filled f -> removeHoles f
    other -> pure other

emptyTS :: TypeSpace
emptyTS = TypeSpace HashMap.empty HashMap.empty HashMap.empty HashMap.empty

addTyKind :: MonadTyper m => Text -> TKind -> TyInfo -> m ()
addTyKind name ki info = do
  curMod <- gets _teCurModule
  modify (over (globalTypingEnv . tsTypes) (HashMap.insert (curMod <> "." <> name) (ki, info)))

updateTyKind :: MonadTyper m => Text -> ((TKind, TyInfo) -> Maybe (TKind, TyInfo)) -> m ()
updateTyKind name f = do
  modify (over (globalTypingEnv . tsTypes) (HashMap.update f name))

addTy :: MonadTyper m => Lens' TypeSpace (HashMap Text b) -> Text -> b -> m ()
addTy lens name ki = do
  curMod <- gets _teCurModule
  modify (over (globalTypingEnv . lens) (HashMap.insert (curMod <> "." <> name) ki))

getTy :: MonadTyper m => Lens' TypeSpace (HashMap Text b) -> Path -> m b
getTy lens path = do
  globalEnv <- gets _globalTypingEnv
  canonPath <- canonicalPath path
  case HashMap.lookup canonPath (view lens globalEnv) of
    Just ty -> pure ty
    Nothing -> terminate (trace (show $ HashMap.keys globalEnv._tsVars) $ NameResolution canonPath)

getTypeSpaceKind ::  Text -> TypeSpace -> Either TypeError TKind
getTypeSpaceKind name' ts =
  let res  = lookup name' (_tsTypes ts)
  in case res of
      Just (kind, _) -> pure kind
      Nothing        -> Left (NameResolution ("Ki" <> name'))

canonicalPath :: MonadTyper m => Path -> m Text
canonicalPath path = do
  curMod <- gets _teCurModule
  pure $ case path of
    (Local name')      -> curMod <> "." <> name'.text
    (Path mod name' _) -> mod <> "." <> name'.text

getKind :: MonadTyper m => Path -> m TKind
getKind path = do
  canonPath <- canonicalPath path
  ts     <- gets _globalTypingEnv
  either terminate pure (getTypeSpaceKind canonPath ts)

addFieldToEnv :: MonadTyper m => Text -> Text -> FieldInfo -> m ()
addFieldToEnv typeName fieldName fieldInfo = do
  curMod <- gets _teCurModule
  modify
    $ over (globalTypingEnv . tsTypeFields)
    $ HashMap.insertWith ((<>)) (curMod <> "." <> typeName) (HashMap.singleton fieldName fieldInfo)

unifyHolesWithStar :: MonadTyper m => TKind -> m TKind
unifyHolesWithStar = \case
  KiStar    -> pure KiStar
  KiFun f t -> KiFun <$> unifyHolesWithStar f <*> unifyHolesWithStar t
  KiHole hole -> do
    content <- readIORef hole
    case content of
      Empty _ _  -> writeIORef hole (Filled KiStar) *> pure KiStar
      Filled res -> unifyHolesWithStar res

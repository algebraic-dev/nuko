module Nuko.Typer.Env (
  TypeSpace(..),
  TyInfo(..),
  FieldInfo(..),
  TypingEnv(..),
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
  updateTyKind
) where

import Relude.Applicative      (Applicative(pure, (<*>), (*>)))
import Relude.Function         ((.), ($))
import Relude.Functor          ((<$>))
import Relude.Monad            (MonadIO, Maybe (..), MonadState, Either (..), either, modify, gets)
import Relude.String           (Text)
import Relude.Monoid           ((<>), Endo)
import Relude.Lifted           (readIORef, writeIORef)
import Relude.Base             (Generic)

import Nuko.Utils              (terminate)
import Nuko.Typer.Types        (Hole (..), TKind (..), TTy (..), Virtual)
import Nuko.Typer.Error        (TypeError(NameResolution))
import Nuko.Resolver.Tree      (Path (..), ReId(text))

import Pretty.Tree             (PrettyTree)
import Data.HashMap.Strict     (HashMap, lookup)
import Control.Monad.Chronicle (MonadChronicle)
import Lens.Micro.Platform     (makeLenses, over, Lens')

import qualified Data.HashMap.Strict as HashMap

data TyInfo
  = IsTySyn
  | IsTyDef
  deriving Generic

data FieldInfo = FieldInfo
  { _fiResultType :: TTy Virtual
  } deriving Generic

data TypeSpace = TypeSpace
  { _tsTypes        :: HashMap Text (TKind, TyInfo)
  , _tsConstructors :: HashMap Text (TTy Virtual)
  , _tsTypeFields   :: HashMap Text (HashMap Text FieldInfo)
  } deriving Generic

data TypingEnv = TypingEnv
  { _teCurModule     :: Text
  , _globalTypinvEnv :: TypeSpace
  } deriving Generic

makeLenses ''FieldInfo
makeLenses ''TypeSpace
makeLenses ''TypingEnv

instance PrettyTree TypingEnv where
instance PrettyTree FieldInfo where
instance PrettyTree TyInfo where
instance PrettyTree TypeSpace where

type MonadTyper m =
  ( MonadIO m
  , MonadState TypingEnv m
  , MonadChronicle (Endo [TypeError]) m
  )

emptyTS :: TypeSpace
emptyTS = TypeSpace HashMap.empty HashMap.empty HashMap.empty

addTyKind :: MonadTyper m => Text -> TKind -> TyInfo -> m ()
addTyKind name ki info = do
  curMod <- gets _teCurModule
  modify (over (globalTypinvEnv . tsTypes) (HashMap.insert (curMod <> "." <> name) (ki, info)))

updateTyKind :: MonadTyper m => Text -> ((TKind, TyInfo) -> Maybe (TKind, TyInfo)) -> m ()
updateTyKind name f = do
  modify (over (globalTypinvEnv . tsTypes) (HashMap.update f name))

addTy :: MonadTyper m => Lens' TypeSpace (HashMap Text b) -> Text -> b -> m ()
addTy lens name ki = do
  curMod <- gets _teCurModule
  modify (over (globalTypinvEnv . lens) (HashMap.insert (curMod <> "." <> name) ki))

getTypeSpaceKind ::  Text -> TypeSpace -> Either TypeError TKind
getTypeSpaceKind name' ts =
  let res  = lookup name' (_tsTypes ts)
  in case res of
      Just (kind, _) -> pure kind
      Nothing        -> Left (NameResolution name')

getKind :: MonadTyper m => Path -> m TKind
getKind path = do
  curMod <- gets _teCurModule
  let canonicalPath =
        case path of
          (Local name') -> curMod <> "." <> name'.text
          (Path mod name' _) -> mod <> "." <> name'.text
  ts     <- gets _globalTypinvEnv
  either terminate pure (getTypeSpaceKind canonicalPath ts)

addFieldToEnv :: MonadTyper m => Text -> Text -> FieldInfo -> m ()
addFieldToEnv typeName fieldName fieldInfo = do
  curMod <- gets _teCurModule
  modify
    $ over (globalTypinvEnv . tsTypeFields)
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

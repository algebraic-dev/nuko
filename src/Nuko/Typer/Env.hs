module Nuko.Typer.Env (
  TypeSpace(..),
  MonadTyper,
  getKind,
  tsTypes,
  addTyKind,
  unifyHolesWithStar,
  localTypeSpace,
  tsConstructors,
  tsFields,
  addTy,
  emptyTS
) where

import Relude.Monad            (MonadIO, MonadReader (ask), Maybe (..), MonadState (get, put, state), Either (..), Monad ((>>=)), either, modify)
import Relude.String           (Text)
import Relude.Monoid           ((<>), Endo)
import Relude.Function         ((.))
import Relude.Functor          ((<$>))
import Relude.Lifted           (readIORef, writeIORef)

import Nuko.Utils              (terminate)
import Nuko.Typer.Types        (convertType, Hole (..), TKind (..), TTy, Virtual)
import Nuko.Typer.Error        (TypeError(NameResolution))
import Nuko.Resolver.Tree      (Path (..), ReId(text))
import Data.HashMap.Strict     (HashMap, lookup)
import Relude.Applicative      (Applicative(pure, (<*>), (*>)))
import Control.Monad.Chronicle (MonadChronicle)
import Lens.Micro.Platform     (makeLenses, over, Lens')

import qualified Data.HashMap.Strict as HashMap
import Relude.Base (Generic)
import Pretty.Tree (PrettyTree)

data TypeSpace = TypeSpace
  { _tsTypes        :: HashMap Text TKind
  , _tsFields       :: HashMap Text (TTy Virtual)
  , _tsConstructors :: HashMap Text (TTy Virtual)
  } deriving Generic

makeLenses ''TypeSpace

instance PrettyTree TypeSpace where

type MonadTyper m =
  ( MonadIO m
  , MonadReader TypeSpace m
  , MonadState TypeSpace m
  , MonadChronicle (Endo [TypeError]) m
  )

emptyTS :: TypeSpace
emptyTS = TypeSpace HashMap.empty HashMap.empty HashMap.empty

addTyKind :: MonadTyper m => Text -> TKind -> m ()
addTyKind name ki = modify (over tsTypes (HashMap.insert name ki))

addTy :: MonadTyper m => Lens' TypeSpace (HashMap Text b) -> Text -> b -> m ()
addTy lens name ki = modify (over lens (HashMap.insert name ki))

getTypeSpaceKind ::  Text -> TypeSpace -> Either TypeError TKind
getTypeSpaceKind name' ts =
  let res  = lookup name' (_tsTypes ts)
  in case res of
      Just res' -> pure res'
      Nothing   -> Left (NameResolution name')

getKind :: MonadTyper m => Path -> m TKind
getKind (Local name')      = get >>= either terminate (pure . convertType) . getTypeSpaceKind name'.text
getKind (Path mod name' _) = ask >>= either terminate (pure . convertType) . getTypeSpaceKind (mod <> "." <> name'.text)

unifyHolesWithStar :: MonadTyper m => TKind -> m TKind
unifyHolesWithStar = \case
  KiStar    -> pure KiStar
  KiFun f t -> KiFun <$> unifyHolesWithStar f <*> unifyHolesWithStar t
  KiHole hole -> do
    content <- readIORef hole
    case content of
      Empty _ _  -> writeIORef hole (Filled KiStar) *> pure KiStar
      Filled res -> unifyHolesWithStar res

localTypeSpace :: MonadTyper m => TypeSpace -> m a -> m (a, TypeSpace)
localTypeSpace ts action = do
  current <- get
  put ts
  result <- action
  typeSpace <- state (\s -> (s, current))
  pure (result, typeSpace)
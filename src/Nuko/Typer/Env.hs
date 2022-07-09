module Nuko.Typer.Env (
  TypeSpace(..),
  TyInfo(..),
  MonadTyper,
  getKind,
  tsTypes,
  addTyKind,
  unifyHolesWithStar,
  localTypeSpace,
  tsConstructors,
  tsFields,
  addTy,
  generalizeTy,
  emptyTS
) where

import Relude.Monad            (MonadIO (liftIO), MonadReader (ask), Maybe (..), MonadState (get, put, state), Either (..), Monad ((>>=)), either, modify, StateT, isNothing, join)
import Relude.String           (Text)
import Relude.Monoid           ((<>), Endo)
import Relude.Function         ((.), ($))
import Relude.Functor          ((<$>))
import Relude.Lifted           (readIORef, writeIORef)

import Nuko.Utils              (terminate)
import Nuko.Typer.Types        (Hole (..), TKind (..), TTy (..), Virtual, Int)
import Nuko.Typer.Error        (TypeError(NameResolution))
import Nuko.Typer.Types        (TyHole)
import Nuko.Resolver.Tree      (Path (..), ReId(text, ReId))

import Data.HashMap.Strict     (HashMap, lookup)
import Relude.Applicative      (Applicative(pure, (<*>), (*>)))
import Control.Monad.Chronicle (MonadChronicle)
import Lens.Micro.Platform     (makeLenses, over, Lens')

import qualified Data.HashMap.Strict as HashMap
import Relude.Base (Generic)
import Pretty.Tree (PrettyTree)
import Relude.Container (IntMap, HashSet)
import qualified Data.IntMap as IntMap
import Control.Monad (sequence)
import Nuko.Syntax.Lexer.Support (ghostRange)
import Nuko.Syntax.Range (emptyRange)
import qualified Control.Monad.State.Strict as State
import qualified Data.HashSet as HashSet
import Relude ((+), Foldable (foldr), show)

data TyInfo
  = IsTySyn
  | IsTyDef
  deriving Generic

data TypeSpace = TypeSpace
  { _tsTypes        :: HashMap Text (TKind, TyInfo)
  , _tsFields       :: HashMap Text (TTy Virtual)
  , _tsConstructors :: HashMap Text (TTy Virtual)
  } deriving Generic

makeLenses ''TypeSpace

instance PrettyTree TyInfo where
instance PrettyTree TypeSpace where

type MonadTyper m =
  ( MonadIO m
  , MonadReader TypeSpace m
  , MonadState TypeSpace m
  , MonadChronicle (Endo [TypeError]) m
  )

emptyTS :: TypeSpace
emptyTS = TypeSpace HashMap.empty HashMap.empty HashMap.empty

addTyKind :: MonadTyper m => Text -> TKind -> TyInfo -> m ()
addTyKind name ki info = modify (over tsTypes (HashMap.insert name (ki, info)))

addTy :: MonadTyper m => Lens' TypeSpace (HashMap Text b) -> Text -> b -> m ()
addTy lens name ki = modify (over lens (HashMap.insert name ki))

getTypeSpaceKind ::  Text -> TypeSpace -> Either TypeError TKind
getTypeSpaceKind name' ts =
  let res  = lookup name' (_tsTypes ts)
  in case res of
      Just (kind, _) -> pure kind
      Nothing        -> Left (NameResolution name')

getKind :: MonadTyper m => Path -> m TKind
getKind (Local name')      = get >>= either terminate pure . getTypeSpaceKind name'.text
getKind (Path mod name' _) = ask >>= either terminate pure . getTypeSpaceKind (mod <> "." <> name'.text)

generalizeTy :: MonadTyper m => TTy Virtual -> m (TTy Virtual)
generalizeTy ty = do
    (res, holes) <- State.runStateT (helper 0 ty) 0
    pure (foldr (\n b -> TyForall ("'" <> show (n :: Int)) (\_ -> b)) res [0..holes])
  where
    helper :: MonadTyper m => Int -> TTy Virtual -> StateT Int m (TTy Virtual)
    helper scope = \case
      TyIdent id   -> pure (TyIdent id)
      TyVar int    -> pure (TyVar int)
      TyForall n f -> do
        _ <- helper (scope + 1) (f (TyIdent (Local (ReId n emptyRange))))
        pure (TyForall n f)
      TyApp a b    -> TyApp <$> helper scope a <*> helper scope b
      TyFun a b    -> TyFun <$> helper scope a <*> helper scope b
      TyHole hole  -> do
        content <- liftIO (readIORef hole)
        case content of
          Empty {} -> do
            curAdd <- State.get
            State.modify (+ 1)
            let ty' = TyVar (scope + curAdd)
            writeIORef hole (Filled ty)
            pure ty'
          Filled a  -> helper scope a


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
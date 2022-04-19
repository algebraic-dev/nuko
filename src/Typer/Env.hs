module Typer.Env
  ( Lvl,
    Name,
    Hole (..),
    Ty (..),
    TyHole,
    Env (..),
    TyperMonad,
    Tracker (..),
    track,
    getTyPos,
    scopeVar,
    scopeVars,
    scopeUp,
    newHole,
    substitute,
    generalize,
    instantiate,
    scopeTy,
    existentialize,
    runEnv,
    named,
    fillHole,
    readHole,
    setHole
  )
where

import Typer.Types
import Expr                   (Expr, Typer, Literal)
import Data.Foldable          (traverse_)
import Data.IORef             (newIORef, writeIORef)
import Data.Map               (Map)
import GHC.IORef              (readIORef)
import Syntax.Range           (Range)
import Data.Set               (Set)
import Syntax.Tree            (Normal)
import Control.Monad.State    (MonadState, StateT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Sequence          (Seq ((:|>)))

import qualified Control.Monad.State as State
import qualified Data.Map            as Map
import qualified Data.Text           as Text
import qualified Data.Set            as Set
import qualified Data.Sequence       as Seq

-- | Useful to track what the type checker did to achieve an error.
--   It helps a lot when writing error messages.
data Tracker
  = InInferExpr (Expr Normal)
  | InInferTy   (Typer Normal)
  | InInferLit  (Literal Normal)
  | InCheck     (Expr Normal) Ty
  | InUnify     Ty Ty
  | InApply     Ty (Expr Normal)
  deriving Show

-- | Stores all the info about the environment of typing, trackers
--   (that helps a lot with error localization) and some cool numbers
--   to generate new variable names.
data Env = Env
  { scope     :: Lvl,
    nameGen   :: Int,
    trackers  :: Seq Tracker,
    variables :: Map Name Ty,
    types     :: Set Name
  }

type TyperMonad m = (MonadState Env m, MonadIO m)

updateTracker :: TyperMonad m => (Seq Tracker -> Seq Tracker) -> m ()
updateTracker f = State.modify (\ctx -> ctx {trackers = f ctx.trackers})

track :: TyperMonad m => Tracker -> m a -> m a
track tracker action = do
    updateTracker (:|> tracker)
    res <- action
    updateTracker (removeLeft)
    pure res
  where
    removeLeft :: Seq a -> Seq a
    removeLeft (a :|> _)   = a
    removeLeft (Seq.Empty) = Seq.empty

-- Scoping and variales

updateVars :: TyperMonad m => (Map Name Ty -> Map Name Ty) -> m ()
updateVars f = State.modify (\ctx -> ctx {variables = f (ctx.variables)})

updateTypes :: TyperMonad m => (Set Name -> Set Name) -> m ()
updateTypes f = State.modify (\ctx -> ctx {types = f (ctx.types)})

scopeVar :: TyperMonad m => Name -> Ty -> m a -> m a
scopeVar name ty action = do
  oldTy <- State.gets variables
  updateVars (Map.insert name ty)
  act <- action
  updateVars (const oldTy)
  pure act

scopeVars :: TyperMonad m => [(Name, Ty)] -> m a -> m a
scopeVars vars action = do
  oldTy <- State.gets variables
  traverse_ (updateVars . uncurry Map.insert) vars
  act <- action
  updateVars (const oldTy)
  pure act

scopeTy :: TyperMonad m => Name -> m a -> m a
scopeTy name action = do
  oldTy <- State.gets types
  updateTypes (Set.insert name)
  act <- action
  updateTypes (const oldTy)
  pure act

scopeUp :: TyperMonad m => m a -> m a
scopeUp action =
  modScope (+ 1) *> action <* modScope (subtract 1)
  where
    modScope :: TyperMonad m => (Int -> Int) -> m ()
    modScope f = State.modify (\s -> s {scope = f s.scope})

newHole :: TyperMonad m => m TyHole
newHole = do
  scope' <- State.gets scope
  name <- newNamed
  res <- liftIO $ newIORef (Empty name scope')
  pure res

newNamed :: TyperMonad m => m Name
newNamed = do
  name <- State.state (\env -> (Text.pack $ "'" ++ show env.nameGen, env { nameGen = env.nameGen + 1}))
  pure name

-- Generalization and instantiation

generalize :: TyperMonad m => Ty -> m Ty
generalize ty = do
    let pos = getTyPos ty
    freeVars <- Set.toList <$> go ty
    pure (foldl (flip $ TyForall pos) ty freeVars)
  where
    go :: TyperMonad m => Ty -> m (Set Name)
    go = \case
      TyRef _ ty'   -> go ty'
      TyRigid _ _ _ -> pure Set.empty
      TyNamed _ _ -> pure Set.empty
      TyFun _ ty' ty'' -> Set.union <$> go ty' <*> go ty''
      TyForall _ _ ty' -> go ty'
      TyHole loc hole -> do
        resHole <- liftIO $ readIORef hole
        case resHole of
          Empty _ hScope -> do
            lvl <- State.gets scope
            if (hScope > lvl) then do
              name <- newNamed
              liftIO $ writeIORef hole (Filled $ TyNamed loc name)
              pure $ Set.singleton name
            else pure Set.empty
          Filled ty' -> go ty'

instantiate :: TyperMonad m => Ty -> m Ty
instantiate = \case
  (TyForall loc binder body) -> do
    hole <- TyHole loc <$> newHole
    pure (substitute binder hole body)
  other -> pure other

-- Lol i dont want to use the word "skolomize"
existentialize :: TyperMonad m => Ty -> m Ty
existentialize = \case
  (TyForall loc binder body) -> do
    lvl <- State.gets scope
    pure (substitute binder (TyRigid loc binder lvl) body)
  other -> pure other


fillHole :: TyperMonad m => TyHole -> Ty -> m ()
fillHole hole ty = liftIO $ writeIORef hole (Filled ty)

readHole :: TyperMonad m => TyHole -> m (Hole Ty)
readHole = liftIO . readIORef

setHole :: TyperMonad m => TyHole -> Hole Ty -> m ()
setHole hole = liftIO . writeIORef hole

named :: TyperMonad m => Range -> Name -> m Ty
named range name = pure (TyNamed (Just range) name)

runEnv :: StateT Env IO a -> Env -> IO a
runEnv action env = State.evalStateT action env
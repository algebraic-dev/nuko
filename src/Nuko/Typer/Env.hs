module Nuko.Typer.Env (
    Env(..),
    Tracker(..),
    Namespace(..),
    Visibility(..),
    Name,
    MonadTyper,
    updateTracker,
    track,
    updateVars,
    scopeVar,
    scopeVars,
    scopeTy,
    scopeUp,
    newHole,
    addTy,
    generalize,
    zonk,
    instantiate,
    existentialize,
    fillHole,
    readHole,
    setHole,
    named,
    runEnv
) where

import Nuko.Typer.Types
import Nuko.Tree.Expr         (Expr, Type, Literal, Pat)
import Nuko.Syntax.Range      (Range, HasPosition (getPos))
import Nuko.Syntax.Ast        (Normal)
import Data.Foldable          (traverse_)
import Data.IORef             (newIORef, writeIORef)
import GHC.IORef              (readIORef)
import Data.Set               (Set)
import Control.Monad.State    (MonadState, StateT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Sequence          (Seq ((:|>)))
import Control.Monad          (when)
import Data.HashMap.Strict    (HashMap)

import qualified Control.Monad.State as State
import qualified Data.Text           as Text
import qualified Data.Set            as Set
import qualified Data.Sequence       as Seq
import qualified Data.HashMap.Strict as HashMap

-- | Useful to track what the type checker did to achieve an error.
--   It helps a lot when writing error messages.
data Tracker
  = InInferExpr (Expr Normal)
  | InInferTy   (Type Normal)
  | InInferLit  (Literal Normal)
  | InInferPat  (Pat Normal)
  | InCheckPat  (Pat Normal)
  | InInferGen  Range
  | InCheck     (Expr Normal) Ty
  | InUnify     Ty Ty
  | InApply     Ty (Expr Normal)
  deriving Show


debug :: Bool
debug = False

data Visibility = Public | Private

-- | This is like a namespace instance. Or a module.. so it holds a lot
--   of info about the types, functions and constants defined inside of them.
--   all the types usually create namespaces to avoid conflicts.

data Namespace = Namespace
  { types :: HashMap Name (Visibility, Ty),
    name  :: Name
  }

-- | This is the typing environment so the info here is volatile in the sense
--   that it not constitutes a module or namespace, it's just the typing context
--   at a moment in the code. Also it stores all the info about the trackers
--   (that helps a lot with error localization)

data Env = Env
  { scope        :: Lvl,
    nameGen      :: Int,
    trackers     :: Seq Tracker,
    variables    :: HashMap Name Ty,
    localTypes   :: HashMap Name Ty,
    curNamespace :: Namespace,
    namespaces   :: HashMap Name Namespace,
    debugLvl     :: Int
  }

type MonadTyper m = (MonadState Env m, MonadIO m)

updateTracker :: MonadTyper m => (Seq Tracker -> Seq Tracker) -> m ()
updateTracker f = State.modify (\ctx -> ctx {trackers = f ctx.trackers})

track :: MonadTyper m => Tracker -> m a -> m a
track tracker action = do
    updateTracker (:|> tracker)
    when debug $ do
      lvl <- State.gets debugLvl
      State.modify (\ctx -> ctx { debugLvl = lvl + 1 })
      liftIO $ putStrLn $ replicate (lvl * 3) ' ' ++ show tracker
    res <- action
    updateTracker (removeLeft)
    when debug $ do
      lvl <- State.gets debugLvl
      State.modify (\ctx -> ctx { debugLvl = lvl - 1 })
    pure res
  where
    removeLeft :: Seq a -> Seq a
    removeLeft (a :|> _)   = a
    removeLeft (Seq.Empty) = Seq.empty

-- Scoping and variales

updateVars :: MonadTyper m => (HashMap Name Ty -> HashMap Name Ty) -> m ()
updateVars f = State.modify (\ctx -> ctx {variables = f (ctx.variables)})

updateTypes :: MonadTyper m => (HashMap Name Ty -> HashMap Name Ty) -> m ()
updateTypes f = State.modify (\ctx -> ctx {localTypes = f (ctx.localTypes)})

scopeVar :: MonadTyper m => Name -> Ty -> m a -> m a
scopeVar name ty action = do
  oldTy <- State.gets variables
  updateVars (HashMap.insert name ty)
  act <- action
  updateVars (const oldTy)
  pure act

scopeVars :: MonadTyper m => [(Name, Ty)] -> m a -> m a
scopeVars vars action = do
  oldTy <- State.gets variables
  traverse_ (updateVars . uncurry HashMap.insert) vars
  act <- action
  updateVars (const oldTy)
  pure act

scopeTy :: MonadTyper m => Name -> Ty -> m a -> m a
scopeTy name ty action = do
  oldTy <- State.gets localTypes
  updateTypes (HashMap.insert name ty)
  act <- action
  updateTypes (const oldTy)
  pure act

scopeUp :: MonadTyper m => m a -> m a
scopeUp action =
  modScope (+ 1) *> action <* modScope (subtract 1)
  where
    modScope :: MonadTyper m => (Int -> Int) -> m ()
    modScope f = State.modify (\s -> s {scope = f s.scope})

newHole :: MonadTyper m => m TyHole
newHole = do
  scope' <- State.gets scope
  name <- newNamed
  res <- liftIO $ newIORef (Empty name scope')
  pure res

newNamed :: MonadTyper m => m Name
newNamed = do
  name <- State.state (\env -> (Text.pack $ "'" ++ show env.nameGen, env { nameGen = env.nameGen + 1}))
  pure name

addTy :: MonadTyper m => Name -> Ty -> m ()
addTy name ty = updateTypes $ HashMap.insert name ty

-- Generalization and instantiation

generalize :: MonadTyper m => Ty -> m Ty
generalize ty = do
    let pos = getPos ty
    freeVars <- Set.toList <$> go ty
    pure (foldl (flip $ TyForall pos) ty freeVars)
  where
    go :: MonadTyper m => Ty -> m (Set Name)
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

zonk :: MonadTyper m => Ty -> m Ty
zonk = \case
  TyRef pos ty'   -> TyRef pos <$> zonk ty'
  TyRigid pos name lvl -> pure $ TyRigid pos name lvl
  TyNamed pos name -> pure $ TyNamed pos name
  TyFun pos ty' ty'' -> TyFun pos <$> zonk ty' <*> zonk ty''
  TyForall pos name ty' -> TyForall pos name <$> zonk ty'
  TyHole loc hole -> do
    resHole <- liftIO $ readIORef hole
    case resHole of
      Empty _ _ -> pure (TyHole loc hole)
      Filled ty' -> zonk ty'

instantiate :: MonadTyper m => Ty -> m Ty
instantiate = \case
  (TyForall loc binder body) -> do
    hole <- TyHole loc <$> newHole
    pure (substitute binder hole body)
  other -> pure other

-- Lol i dont want to use the word "skolomize"
existentialize :: MonadTyper m => Ty -> m Ty
existentialize = \case
  (TyForall loc binder body) -> do
    lvl <- State.gets scope
    pure (substitute binder (TyRigid loc binder lvl) body)
  other -> pure other

fillHole :: MonadTyper m => TyHole -> Ty -> m ()
fillHole hole ty = liftIO $ writeIORef hole (Filled ty)

readHole :: MonadTyper m => TyHole -> m (Hole Ty)
readHole = liftIO . readIORef

setHole :: MonadTyper m => TyHole -> Hole Ty -> m ()
setHole hole = liftIO . writeIORef hole

named :: MonadTyper m => Range -> Name -> m Ty
named range name = pure (TyNamed range name)

runEnv :: StateT Env IO a -> Env -> IO (a, Env)
runEnv action env = State.runStateT action env
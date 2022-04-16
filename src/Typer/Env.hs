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
    lookupVar,
    getTyPos,
    scopeVar,
    scopeVars,
    scopeUp,
    newHole,
    substitute,
    generalize,
    instantiate,
    scopeTy,
    existentialize
  )
where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState)
import Data.Foldable (traverse_)
import Data.IORef (newIORef, writeIORef)
import Data.Map (Map)
import GHC.IORef (readIORef)
import Syntax.Range (Loc (..), Range)
import Typer.Types 

import qualified Control.Monad.State as State
import qualified Data.Map as Map
import qualified Typer.Error as Err
import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.Set (Set)

{- 
    Things to help me :D
    - https://okmij.org/ftp/ML/generalization.html#generalization
    - https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/putting.pdf
    - https://gist.github.com/mb64/87ac275c327ea923a8d587df7863d8c7
    - https://arxiv.org/pdf/1306.6032.pdf
-}

-- Just some aliases to help me later

data Tracker
  = TrCheck Range Loc
  | TrInfer Range 
  | TrUnify Loc Loc

data Env = Env
  { scope     :: Lvl,
    nameGen   :: Int,
    trackers  :: [Tracker],
    variables :: Map Name Ty,
    types     :: Set Name
  }

type TyperMonad m = (MonadState Env m, MonadIO m)

-- Things for tracking the last things to error reporting

updateTracker :: TyperMonad m => ([Tracker] -> [Tracker]) -> m ()
updateTracker f = State.modify (\ctx -> ctx {trackers = f ctx.trackers})

track :: TyperMonad m => Tracker -> m a -> m a
track tracker action = updateTracker (tracker :) *> action <* updateTracker tail

-- Scoping and variales

lookupVar :: TyperMonad m => Name -> m Ty
lookupVar name = do
  res <- State.gets (Map.lookup name . variables)
  maybe (liftIO $ throwIO (Err.CannotFind name)) pure res

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
  name <- State.state (\env -> (Text.pack $ "'" ++ show env.nameGen, env))
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
    pure (substitute binder body hole)
  other -> pure other

existentialize :: TyperMonad m => Ty -> m Ty 
existentialize = \case  
  (TyForall loc binder body) -> do
    lvl <- State.gets scope
    pure (substitute binder body $ TyRigid loc binder lvl)
  other -> pure other
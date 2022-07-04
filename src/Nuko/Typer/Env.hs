module Nuko.Typer.Env (
  MonadTyper,
  Tracker(..),
  TypeSpace(..),
  TypingEnv(..),
  Env(..),
  Name,
  tsTypes,
  tsConst,
  tsVars,
  tsFields,
  tsName,
  trackers,
  importedVars,
  typeNames,
  teScope,
  variables,
  addTy,
  addVar,
  lookupVar,
  instantiate,
  emptyEnv,
  emptyTypingEnv
) where

import Nuko.Tree               (Expr, Pat, Literal, Ty, Nm)
import Nuko.Typer.Types        (Name, Lvl, TTy (..), Virtual, Hole (..))
import Nuko.Syntax.Range       (Range)
import Nuko.Typer.Error        (TypeError)

import Relude.Monad            (MonadState, MonadIO, Maybe(..))
import Relude                  (MonadReader (local), Num ((+)), (.), ($), asks, Applicative (pure), newIORef)
import Relude.Numeric          (Int)
import Relude.Container        (Seq, HashSet, IntMap)
import Control.Monad.Chronicle (MonadChronicle)
import Data.Semigroup          (Endo)
import Data.Text               (Text)
import Data.HashMap.Strict     (HashMap)
import Lens.Micro.Platform     (makeLenses, over)
import GHC.Err                 (error)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap         as IntMap
import qualified Data.Sequence as Seq
import Control.Monad.Import (MonadImport)

-- | Useful to track what the type checker did to achieve an error.
--   It helps a lot when writing error messages.

data Tracker
  = InInferExpr (Expr Nm)
  | InInferTy   (Ty Nm)
  | InInferLit  (Literal Nm)
  | InInferPat  (Pat Nm)
  | InCheckPat  (Pat Nm)
  | InInferGen  Range
  | InCheck     (Expr Nm) (TTy Virtual)
  | InUnify     (TTy Virtual) (TTy Virtual)
  | InApply     (TTy Virtual) (Expr Nm)

-- | This is like the NameSpace in the resolver module but it maps to types.
-- It is generated in the end of the type checking phase

data TypeSpace = TypeSpace
  { _tsTypes  :: HashSet Name
  , _tsConst  :: HashMap Name (TTy Virtual)
  , _tsVars   :: HashMap Name (TTy Virtual)
  , _tsFields :: HashMap Name (TTy Virtual)
  , _tsName   :: Name
  }

makeLenses 'TypeSpace

data Env = Env
  { _trackers     :: Seq Tracker
  , _importedVars :: HashMap Name (TTy Virtual)
  }

emptyEnv :: Env
emptyEnv = Env Seq.empty HashMap.empty

makeLenses 'Env

data TypingEnv = TypingEnv
  { _typeNames :: IntMap Name
  , _teScope   :: Lvl
  , _variables :: HashMap Text (TTy Virtual)
  }

emptyTypingEnv :: TypingEnv
emptyTypingEnv = TypingEnv IntMap.empty 0 HashMap.empty

makeLenses 'TypingEnv

-- | The main Monad for type checking

type MonadTyper m =
  ( MonadState Env m
  , MonadReader TypingEnv m
  , MonadImport TypeSpace m
  , MonadChronicle (Endo [TypeError]) m
  , MonadIO m
  )

addTy :: MonadTyper m => Name -> m a -> m a
addTy name = local $ over typeNames (\s -> IntMap.insert (IntMap.size s) name s)
                   . over teScope (+ 1)

addVar :: MonadTyper m => Name -> (TTy Virtual) -> m a -> m a
addVar name val = local $ over variables (HashMap.insert name val)

lookupVar :: MonadTyper m => Name -> m (TTy Virtual)
lookupVar name = do
  result <- asks (HashMap.lookup name . _variables)
  case result of
    Just res -> pure res
    Nothing  -> error "Type checker: Lol the resolver failed!"

instantiate :: MonadTyper m => TTy Virtual -> m (TTy Virtual)
instantiate = \case
  TyForall name ty -> do
    scope <- asks _teScope
    hole  <- newIORef (Empty name scope)
    instantiate (ty (TyHole hole))
  a -> pure a
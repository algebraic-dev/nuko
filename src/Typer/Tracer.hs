module Typer.Tracer (
    TyperState(..),
    Step(..),
    MonadTyper,
    scope,
    firstUnifyStep,
    lastUnifyStep,
    getInnermostType,
    emptyTracer
) where

import Control.Monad.State (MonadState)
import Data.Sequence (Seq)

import Syntax.Parser.Ast (Normal)
import Typer.Types (EvalStatus(Eval), Kind)

import qualified Syntax.Expr as Expr
import qualified Control.Monad.State as State
import qualified Data.Sequence as Seq
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (isJust)

newtype TyperState = TyperState
    { typerSteps :: Seq Step
    }

data Step
    = InferKind (Expr.Typer Normal)
    | CheckKind (Expr.Typer Normal) (Kind 'Eval)
    | UnifyKind (Kind 'Eval) (Kind 'Eval)

emptyTracer :: TyperState
emptyTracer = TyperState Seq.Empty

type MonadTyper m = (MonadState TyperState m, MonadIO m)

scope :: MonadTyper m => Step -> m a -> m a
scope step action = do
    cur <- State.gets typerSteps
    State.modify (\s -> s { typerSteps = cur Seq.|> step })
    res <- action
    State.modify (\s -> s { typerSteps = cur })
    pure res

getInnermostType :: MonadTyper m => m (Expr.Typer Normal)
getInnermostType = do
  steps <- State.gets typerSteps
  pure (go steps)
  where
    go = \case
      (_    Seq.:|> InferKind e) -> e
      (_    Seq.:|> CheckKind e _) -> e
      (seq' Seq.:|> UnifyKind {}) -> go seq'
      Seq.Empty -> error "Impossible in getInnermostType"


findStepL :: MonadTyper m => (Step -> Maybe b) -> m (Maybe b)
findStepL fn = do
    steps <- State.gets typerSteps
    let step = Seq.index steps <$> Seq.findIndexL (isJust . fn) steps
    pure (step >>= fn)

findStepR :: MonadTyper m => (Step -> Maybe b) -> m (Maybe b)
findStepR fn = do
    steps <- State.gets typerSteps
    let step = Seq.index steps <$> Seq.findIndexR (isJust . fn) steps
    pure (step >>= fn)

isUnifyStep :: Step -> Maybe (Kind 'Eval, Kind 'Eval)
isUnifyStep = \case
    UnifyKind a b -> Just (a,b)
    _ -> Nothing

firstUnifyStep :: MonadTyper m => m (Maybe (Kind 'Eval, Kind 'Eval))
firstUnifyStep = findStepL isUnifyStep

lastUnifyStep :: MonadTyper m => m (Maybe (Kind 'Eval, Kind 'Eval))
lastUnifyStep = findStepR isUnifyStep



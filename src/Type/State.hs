module Type.State where

import Type.Context ((|>), typeWellFormed)
import Control.Monad.State (MonadState)
import Control.Monad.Except (MonadError (throwError))

import qualified Data.Text            as Text
import qualified Type.Context         as Context
import qualified Type.Types           as Types
import qualified Control.Monad.State  as State
import qualified Data.Sequence        as Sequence
import Data.Functor (($>))

data TypingContext =
  TypingContext { typeCtx :: Context.Ctx
                , nameCtx :: [String]
                }

type Typer m = (MonadState TypingContext m, MonadError String m)

modify :: Typer m => (Context.Ctx -> Context.Ctx) -> m ()
modify fun = State.modify' $ \s -> s { typeCtx = fun (typeCtx s) }

dropUntil :: Typer m => Context.CtxElem -> m ()
dropUntil elm = modify (`Context.dropUntil` elm)

mark :: Typer m => Context.CtxElem -> m a -> m a
mark elm action = modify (|> elm) *> action <* dropUntil elm

markList :: Typer m => Context.CtxElem  -> [Context.CtxElem] -> m a -> m a
markList elm more action = add (elm : more) *> action <* dropUntil elm

add :: Typer m => [Context.CtxElem] -> m ()
add elm = modify (\(Context.Ctx x) -> Context.Ctx $ x Sequence.>< Sequence.fromList elm)

freshName :: Typer m => m Text.Text
freshName = State.gets nameCtx >>=
  \case []       -> error "FATAL ERROR: No fresh names available"
        (x : xs) -> State.modify (\s -> s { nameCtx = xs })
                 $> Text.pack x

local :: Typer m => Context.Ctx -> m a -> m a
local ctx action = do
  lastCtx <- State.gets typeCtx
  modify (const ctx)
  res <- action
  modify (const lastCtx)
  pure res

getCtx :: Typer m => m Context.Ctx 
getCtx = State.gets typeCtx

replace :: Typer m => Context.CtxElem -> [Context.CtxElem] -> m ()
replace from to = modify (\s -> Context.replaceCtx s from to)

isWellFormed :: Typer m => Types.Type a -> m ()
isWellFormed ty = do 
  ctx <- State.gets typeCtx
  if typeWellFormed ctx ty 
      then pure ()
      else throwError "Lol type not well formed"

applyCtx :: Typer m => Types.Type 'Types.Poly -> m (Types.Type 'Types.Poly)
applyCtx ty = State.gets ((`Context.applyCtx` ty) . typeCtx)
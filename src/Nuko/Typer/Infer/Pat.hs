module Nuko.Typer.Infer.Pat (
  inferPat
) where

import Relude.Debug       (undefined)
import Relude.Container   (HashMap)
import Relude.Applicative (pure)
import Relude.String      (Text)

import Nuko.Typer.Env   (MonadTyper)
import Nuko.Tree.Expr   (Pat(..))
import Nuko.Tree        (Re, Tc)
import Nuko.Typer.Types (TTy, Virtual)
import Control.Monad.State (StateT)

import qualified Control.Monad.State as State
import qualified Data.HashMap.Strict as HashMap

inferPat :: MonadTyper m => Pat Re -> m (Pat Tc, HashMap Text (TTy Virtual), TTy Virtual)
inferPat pat = do
    ((pat', ty), t) <- State.runStateT (go pat) HashMap.empty
    pure (pat', t, ty)
  where
    go :: MonadTyper m => Pat Re -> StateT (HashMap Text (TTy Virtual)) m (Pat Tc, TTy Virtual)
    go = \case
      PWild _          -> undefined
      PId path _       -> undefined
      PCons path arg _ -> undefined
      PLit lit _       -> undefined
      PAnn pat tty _   -> undefined
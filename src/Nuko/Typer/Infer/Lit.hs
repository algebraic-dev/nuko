module Nuko.Typer.Infer.Lit (
  inferLit
) where

import Nuko.Tree          (Re, Literal(..))
import Nuko.Typer.Env     (MonadTyper)
import Nuko.Typer.Types   (TTy (..), Virtual)
import Relude.Applicative (Applicative(..))

inferLit :: MonadTyper m => Literal Re -> m (TTy Virtual)
inferLit = \case
  LStr _ _ -> pure (TyIdent "Prelude" "String")
  LInt _ _ -> pure (TyIdent "Prelude" "Int")

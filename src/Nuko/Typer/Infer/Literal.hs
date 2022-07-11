module Nuko.Typer.Infer.Literal (
  inferLit
) where

import Nuko.Typer.Env   (MonadTyper)
import Nuko.Tree.Expr   (Literal(..))
import Nuko.Tree        (Re, Tc)
import Nuko.Typer.Types (TTy (TyIdent), Virtual)
import Nuko.Typer.Tree  ()
import Nuko.Resolver.Tree (Path(Path), ReId (ReId))
import Relude.Applicative (Applicative(pure))

inferLit :: MonadTyper m => Literal Re -> m (Literal Tc, TTy Virtual)
inferLit = \case
  LStr l e -> pure (LStr l e, TyIdent (Path "Prelude" (ReId "String" e) e))
  LInt l e -> pure (LInt l e, TyIdent (Path "Prelude" (ReId "Int" e) e))
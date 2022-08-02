module Nuko.Typer.Infer.Literal (
  inferLit,
  boolTy,
  intTy,
  strTy,
  preludeQual,
) where

import Relude

import Nuko.Names       (ModName, Name (..), Qualified (..), TyName, genIdent,
                         mkModName, mkQualifiedWithPos, mkTyName)
import Nuko.Tree        (Re, Tc)
import Nuko.Tree.Expr   (Literal (..))
import Nuko.Typer.Env   (MonadTyper)
import Nuko.Typer.Tree  ()
import Nuko.Typer.Types (Relation (..), TTy (TyIdent))

preludePath :: ModName
preludePath = mkModName (fromList [genIdent "Prelude"])

nameTy :: Text -> Name TyName
nameTy = mkTyName . genIdent

-- This is a placeholder.. in the future i'll try to make something
-- to inject a module as the Prelude.
preludeQual :: Text -> Qualified (Name TyName)
preludeQual = mkQualifiedWithPos preludePath . nameTy

preludeTy :: Text -> TTy k
preludeTy = TyIdent . preludeQual

intTy :: TTy k
intTy = preludeTy "Int"

strTy :: TTy k
strTy = preludeTy "String"

boolTy :: TTy k
boolTy = preludeTy "Bool"

inferLit :: MonadTyper m => Literal Re -> m (Literal Tc, TTy 'Virtual)
inferLit = \case
  LStr l e -> pure (LStr l (strTy, e), strTy)
  LInt l e -> pure (LInt l (intTy, e), intTy)

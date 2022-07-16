module Nuko.Typer.Infer.Literal (
  inferLit,
  boolTy,
  intTy,
  strTy,
  preludeQual,
) where

import Relude.Applicative (Applicative(pure))
import Relude.Container   (fromList)
import Relude             (Maybe(Just), Text, (.))

import Nuko.Typer.Tree    ()
import Nuko.Typer.Env     (MonadTyper)
import Nuko.Typer.Types   (TTy (TyIdent), Relation (..))
import Nuko.Tree.Expr     (Literal(..))
import Nuko.Tree          (Re, Tc)
import Nuko.Names         (mkModName, genIdent, ModName, mkTyName, TyName, Name(..), mkQualifiedWithPos, Qualified (..))

preludePath :: ModName
preludePath = mkModName (fromList [genIdent "Prelude"])

nameTy :: Text -> Name TyName
nameTy = mkTyName . genIdent

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
  LStr l e -> pure (LStr l e, strTy)
  LInt l e -> pure (LInt l e, intTy)
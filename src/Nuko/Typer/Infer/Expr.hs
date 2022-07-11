module Nuko.Typer.Infer.Expr (
  inferExpr,
  checkExpr,
  inferBlock
) where

import Nuko.Typer.Tree    ()
import Nuko.Typer.Env     (MonadTyper, removeHoles)
import Nuko.Tree.Expr     (Expr(..), Block(..), NoExt(NoExt))
import Nuko.Tree          (Re, Tc)
import Nuko.Typer.Types   (TTy(TyFun, TyForall), Virtual )
import Relude.Debug       (undefined)
import Relude.Applicative (Applicative(pure))

import Nuko.Typer.Infer.Literal (inferLit)

inferBlock :: MonadTyper m => Block Re -> m (Block Tc, TTy Virtual)
inferBlock = \case
  BlBind expr rest -> do
    (resExpr,   resTy) <- inferExpr expr
    (resBlock, resTy') <- inferBlock rest
    pure (BlBind resExpr resBlock, resTy')
  BlVar var rest   -> do
    (resBlock, resTy') <- inferBlock rest
    pure (BlVar undefined resBlock, resTy')
  BlEnd expr       -> do
    (resExpr, resTy) <- inferExpr expr
    pure (BlEnd resExpr, resTy)

checkExpr :: MonadTyper m => Expr Re -> TTy Virtual -> m (Expr Tc, TTy Virtual)
checkExpr expr tty = do
  sanitized <- removeHoles tty
  case (expr, sanitized) of
    (_, TyForall n a)           -> undefined
    (Lam pat expr _, TyFun a b) -> undefined
    (_, a)                      -> undefined

inferExpr :: MonadTyper m => Expr Re -> m (Expr Tc, TTy Virtual)
inferExpr = \case
  Lit lit _ -> do
    (resLit, resTy) <- inferLit lit
    pure (Lit resLit NoExt, resTy)
  Lam pat expr _   -> undefined
  App expr args _  -> undefined
  Lower path _     -> undefined
  Upper path _     -> undefined
  Field expr f _   -> undefined
  If con if' e _   -> undefined
  Match scrt cas _ -> undefined
  Ann exp ty _     -> undefined
  Block block _    -> undefined
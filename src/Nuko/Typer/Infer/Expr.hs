module Nuko.Typer.Infer.Expr (
  inferExpr,
  checkExpr,
  inferBlock
) where

import Relude               (fst, asks, Monad ((>>=)), readIORef, Maybe (Just, Nothing))
import Relude.Lifted        (writeIORef, newIORef)
import Relude.Debug         (undefined)
import Relude.Applicative   (Applicative(pure))
import Relude.Function      (($))
import Relude.Functor       ((<$>))
import Relude.List.NonEmpty (NonEmpty((:|)))

import Nuko.Typer.Env
import Nuko.Resolver.Tree       (Path(..), ReId (..))
import Nuko.Typer.Tree          ()
import Nuko.Typer.Types         (TTy(..), Virtual, TKind (KiStar), dereferenceType, Hole (..) )
import Nuko.Typer.Infer.Literal (inferLit)
import Nuko.Typer.Infer.Pat     (inferPat)
import Nuko.Typer.Unify         (unify, unifyKind)
import Nuko.Typer.Infer.Type    (inferTy)
import Nuko.Typer.Error         (TypeError(NotAFunction))
import Nuko.Tree.Expr           (Expr(..), Block(..), NoExt(NoExt), Var (..))
import Nuko.Utils               (terminate)
import Nuko.Tree                (Re, Tc)

import Data.List.NonEmpty ((<|))
import Data.Traversable   (for, Traversable (traverse))
import Control.Monad      (foldM)
import Nuko.Report.Range

inferBlock :: MonadTyper m => Block Re -> m (Block Tc, TTy Virtual)
inferBlock = \case
  BlBind expr rest -> do
    (resExpr,       _) <- inferExpr expr
    (resBlock, resTy') <- inferBlock rest
    pure (BlBind resExpr resBlock, resTy')
  BlVar var rest   -> do
    (patRes, bindings, patTy) <- inferPat var.pat
    (exprRes, valTy) <- inferExpr var.val
    unify valTy patTy
    (resBlock, resTy') <- addLocals bindings (inferBlock rest)
    pure (BlVar (Var patRes exprRes var.ext) resBlock, resTy')
  BlEnd expr       -> do
    (resExpr, resTy) <- inferExpr expr
    pure (BlEnd resExpr, resTy)

checkExpr :: MonadTyper m => Expr Re -> TTy Virtual -> m (Expr Tc)
checkExpr expr tty = do
  sanitized <- removeHoles tty
  case (expr, sanitized) of
    (_, TyForall _ f) -> do
      ctxLvl <- asks _seScope
      checkExpr expr (f (TyVar ctxLvl))
    (Lam pat expr' e, TyFun argTy retTy) -> do
      (patRes, bindings, patTy) <- inferPat pat
      unify patTy argTy
      exprRes <- addLocals bindings (checkExpr expr' retTy)
      pure (Lam patRes exprRes e)
    _ -> do
      (resExpr, inferedTy) <- inferExpr expr
      unify inferedTy tty
      pure resExpr

applyExpr :: MonadTyper m => TTy Virtual -> Expr Re -> m (Expr Tc, TTy Virtual)
applyExpr toCheck arg = do
  instTy <- eagerInstantiate toCheck >>= dereferenceType
  case instTy of
    TyFun argTy retTy -> do
      argRes <- checkExpr arg argTy
      pure (argRes, retTy)
    TyHole hole -> do
      content <- readIORef hole
      case content of
        Empty n scope -> do
          tyArg <- TyHole <$> newIORef (Empty n scope)
          tyRes <- TyHole <$> newIORef (Empty n scope)
          writeIORef hole (Filled (TyFun tyArg tyRes))
          resExpr <- checkExpr arg tyArg
          pure (resExpr, tyRes)
        Filled _ -> terminate NotAFunction
    _ -> terminate NotAFunction

inferExpr :: MonadTyper m => Expr Re -> m (Expr Tc, TTy Virtual)
inferExpr = \case
  Field expr f _   -> undefined
  If con if' e ext -> do
    (conRes, conTy) <- inferExpr con
    (if'Res, ifTy)  <- inferExpr if'
    unify conTy (TyIdent (Path "Prelude" (ReId "Bool" emptyRange) emptyRange))
    eRes <- traverse (\e' -> checkExpr e' ifTy) e
    pure (If conRes if'Res eRes ext, ifTy)
  Lit lit _ -> do
    (resLit, resTy) <- inferLit lit
    pure (Lit resLit NoExt, resTy)
  Lam pat expr e   -> do
    (patRes, bindings, patTy) <- inferPat pat
    (exprRes, resTy) <- addLocals bindings (inferExpr expr)
    instTy <- eagerInstantiate resTy
    pure (Lam patRes exprRes e, TyFun patTy instTy)
  App expr (arg :| args) e  -> do
    (exprRes, fnTy)  <- inferExpr expr
    (argRes, appTy)  <- applyExpr fnTy arg
    (argsRs, resTy') <- foldM (\(resArgs, resTy) arg' -> (\(newArg, resTy') -> (newArg <| resArgs, resTy')) <$> applyExpr resTy arg') (argRes :| [], appTy) args
    pure (App exprRes argsRs e, resTy')
  Lower path e     ->
    (Lower path e,) <$> case path of
      Local r    -> getLocal r.text
      _          -> getTy tsVars path
  Upper path e     ->
    (Upper path e,) <$> case path of
      Local r    -> getLocal r.text -- Impossible case
      _          -> fst <$> getTy (tsConstructors) path
  Match scrut cas e -> do
    (scrutRes, scrutTy) <- inferExpr scrut
    resTy <- newTyHole "?"
    casesRes <- for cas $ \(pat, expr) -> do
      (resPat, bindings, patTy) <- inferPat pat
      (resExpr, exprTy) <- addLocals bindings (inferExpr expr)
      unify scrutTy patTy
      unify resTy exprTy
      pure (resPat, resExpr)
    pure (Match scrutRes casesRes e, resTy)
  Ann exp ty _     -> do
    (resTy, resKind) <- inferTy [] ty
    unifyKind resKind KiStar
    exprRes <- checkExpr exp resTy
    pure (exprRes, resTy)
  Block block e    -> do
    (blockRes, resTy) <- inferBlock block
    pure (Block blockRes e, resTy)
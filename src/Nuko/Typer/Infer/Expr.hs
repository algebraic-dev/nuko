module Nuko.Typer.Infer.Expr (
  inferExpr,
  checkExpr
) where

import Nuko.Tree
import Nuko.Typer.Env

import Pretty.Tree           (PrettyTree(prettyShowTree) )
import Control.Monad         (foldM)

import Relude.Applicative    (Applicative ((*>), pure))
import Relude.Functor        ((<$>))
import Relude.Lifted         (print, writeIORef, readIORef, newIORef)
import Relude.Monad          (asks, (=<<))
import Relude                (error, ($), putTextLn)

import Nuko.Resolver.Tree    (Path(Local), ReId(text))
import Nuko.Typer.Types      (TTy (..), Virtual, removeHole, Hole (..))
import Nuko.Typer.Error      (ErrCause(NotAFunction), TypeError (..))
import Nuko.Typer.Unify      (unify)
import Nuko.Utils            (terminate)

import Nuko.Typer.Infer.Type (inferTy)
import Nuko.Typer.Infer.Lit (inferLit)

checkExpr :: MonadTyper m => Expr Re -> TTy Virtual -> m ()
checkExpr expr ty = case (expr, ty) of
  (_, TyForall name fn) -> do
    putTextLn (prettyShowTree expr)
    print ty
    curScope <- asks _teScope
    addTy name $ do
      print (fn (TyVar curScope))
      checkExpr expr (fn (TyVar curScope))
  (Lam (PId id _) exp _, TyFun f t) -> do
    addVar id.text f $ checkExpr exp t
  _ -> do
    infered <- instantiate =<< inferExpr expr
    print (infered, ty)
    unify infered ty

inferBlock :: MonadTyper m => Block Re -> m (TTy Virtual)
inferBlock = \case
  BlBind bef af -> inferExpr bef *> inferBlock af
  BlVar (Var (PId x _) val _) af  -> do
    exprTy <- inferExpr val
    addVar x.text exprTy $ inferBlock af
  BlEnd expr    -> inferExpr expr
  _ -> error "Not implemented yet!"

inferExpr :: MonadTyper m => Expr Re -> m (TTy Virtual)
inferExpr = \case
  Lit lit _            -> inferLit lit
  Lower (Local reId) _ -> lookupVar reId.text
  Ann expr ty _        -> do
    ty' <- inferTy ty
    checkExpr expr ty'
    pure ty'
  App fun args      _  -> do
    funTy <- removeHole =<< instantiate =<< inferExpr fun
    foldM applyExpr funTy args
  Lam (PId id _) exp _ -> do
    curScope <- asks _teScope
    argTy <- TyHole <$> newIORef (Empty id.text curScope)
    resTy <- addVar id.text argTy (instantiate =<< inferExpr exp)
    pure (TyFun argTy resTy)
  Block block      _   -> inferBlock block
  _ -> error "Not implemented yet!"

applyExpr :: MonadTyper m => TTy Virtual -> Expr Re -> m (TTy Virtual)
applyExpr ty expr = case ty of
  TyFun fTy tTy -> do checkExpr expr fTy; pure tTy
  TyHole hole   -> do
    content <- readIORef hole
    case content of
      Empty name scope -> do
        fromTy <- TyHole <$> newIORef (Empty name scope)
        toTy   <- TyHole <$> newIORef (Empty name scope)
        writeIORef hole (Filled (TyFun fromTy toTy))
        checkExpr expr fromTy
        pure toTy
      Filled res -> applyExpr res expr
  _ -> terminate (TypeError $ NotAFunction ty)

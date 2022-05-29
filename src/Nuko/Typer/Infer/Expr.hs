module Nuko.Typer.Infer.Expr (
  inferExpr
) where

import Nuko.Typer.Env
import Nuko.Tree.Expr         hiding (Name)
import Nuko.Syntax.Range      (getPos, Range)
import Nuko.Syntax.Ast        (Normal)
import Nuko.Typer.Unify       (unify)
import Nuko.Typer.Types       (isFilled, getFilled, Ty (..), Hole (..))
import Nuko.Typer.Error       (typeError, ErrCause (NotAFunction))
import Control.Monad          (foldM)

import Nuko.Typer.Infer.Common (inferPattern, checkPattern)
import Nuko.Typer.Infer.Pat    (inferLit)
import Nuko.Typer.Infer.Type   (inferTy)

import qualified Data.Map            as Map
import qualified Data.List.NonEmpty  as NonEmpty

unifyFun :: MonadTyper m => Ty -> Range -> Range -> m (Ty, Ty)
unifyFun (TyFun _ a b) _ _ = pure (a,b)
unifyFun tau arg ret = do
  argTy <- TyHole arg <$> newHole
  retTy <- TyHole ret <$> newHole
  unify tau (TyFun (arg <> ret) argTy retTy)
  pure (argTy, retTy)

checkExpr :: MonadTyper m => Expr Normal -> Ty -> m ()
checkExpr expr ty = track (InCheck expr ty) $ case (expr, ty) of
  (_, TyRef _ ty') -> checkExpr expr ty'
  (_, TyHole _ hole) | isFilled hole -> checkExpr expr (getFilled hole)
  (_, TyForall r binder _) -> do
    ext <- existentialize ty
    scopeUp $ scopeTy binder (TyNamed r binder) (checkExpr expr ext)
  (Lam param body _, tau) -> do
    (argTy, retTy) <- unifyFun tau (getPos param) (getPos body)
    vars <- checkPattern param argTy Map.empty
    scopeVars (Map.toList vars) (checkExpr body retTy)
  (expr', ty') -> do
    inferred <- inferExpr expr'
    unify inferred ty'

inferSttms :: MonadTyper m => Block Normal -> m Ty
inferSttms (BlEnd expr) = inferExpr expr
inferSttms (BlBind expr next) = do
  _ <- inferExpr expr
  inferSttms next

inferSttms (BlVar (Var pat expr _) next) = do
  bodyTy <- inferExpr expr
  (argTy, newVars) <- inferPattern pat Map.empty
  unify argTy bodyTy
  traversedMap <- traverse generalize newVars
  scopeVars (Map.toList traversedMap)
    (inferSttms next)

inferExpr :: MonadTyper m => Expr Normal -> m Ty
inferExpr expr = track (InInferExpr expr) $ do
  case expr of
    Lam pat body loc' -> do
      (argTy, vars) <- inferPattern pat Map.empty
      retTy <- scopeVars (Map.toList vars) (inferExpr body)
      pure $ TyFun loc' argTy retTy
    Lit lit _ -> inferLit lit
    App fun (a NonEmpty.:| r) _ -> do
      funTy <- inferExpr fun
      foldM applyExpr funTy (a : r)
    Ann expr' ty _ -> do
      iTy <- inferTy ty
      checkExpr expr' iTy
      pure iTy
    Block sttms _ -> inferSttms sttms
    ast -> error $ "Not implemented yet " ++ show ast

applyExpr :: MonadTyper m => Ty -> Expr Normal -> m Ty
applyExpr ty expr = track (InApply ty expr) $ case ty of
  TyFun _ arg ret -> checkExpr expr arg >> pure ret
  TyForall _ _ _  -> instantiate ty >>= (`applyExpr` expr)
  TyRef _ ty' -> applyExpr ty' expr
  TyHole loc' hole -> do
    resHole <- readHole hole
    case resHole of
      Empty _ _ -> do
        argHole <- TyHole loc' <$> newHole
        retHole <- TyHole loc' <$> newHole
        fillHole hole (TyFun loc' argHole retHole)
        checkExpr expr argHole
        pure retHole
      Filled t -> applyExpr t expr
  other -> typeError (NotAFunction other)
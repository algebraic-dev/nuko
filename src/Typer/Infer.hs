module Typer.Infer (
    inferLit,
    inferTy,
    inferExpr
) where

import Typer.Env
import Expr
import Syntax.Range           (getPos, Range)
import Syntax.Tree            (Normal)
import Typer.Unify            (unify)
import Typer.Types            (isFilled, getFilled)
import Data.Set               (Set)
import Typer.Error            (typeError, ErrCause (NotAFunction, CannotFindType))
import Control.Monad          (when)

import qualified Data.Set            as Set
import qualified Data.Map            as Map
import qualified Typer.Error         as Err
import qualified Typer.Types         as Typer
import qualified Control.Monad.State as State

lookupVar :: TyperMonad m => Range -> Typer.Env.Name -> m Ty
lookupVar loc name = do
  res <- State.gets (Map.lookup name . variables)
  maybe (typeError (Err.CannotFind loc name))
        (\ty -> pure $ maybe (TyRef (Just loc) ty) (const ty) $ getTyPos ty)
        res

checkTy :: TyperMonad m => Range -> Typer.Name -> m ()
checkTy range name = do
  notMember <- State.gets (Set.notMember name . types)
  when notMember (typeError (CannotFindType range name))

inferLit :: TyperMonad m => Literal Normal -> m Ty
inferLit lit = track (InInferLit lit) $ case lit of
  LString loc _ -> named loc "String"
  LChar loc   _ -> named loc "Char"
  LInt loc    _ -> named loc "Int"
  LDouble loc _ -> named loc "Double"

inferTy :: TyperMonad m => Typer Normal -> m Ty
inferTy t = do
    let loc = Just (getPos t)
    (ty, unbounded) <- go t
    pure (foldl (flip $ TyForall loc) ty unbounded)
  where
    go :: TyperMonad m => Typer Normal -> m (Ty, Set Typer.Env.Name)
    go ty = track (InInferTy ty) $ case ty of
      TSimple _ name       -> do
        checkTy name.loc name.ident
        pure (TyNamed (Just name.loc) name.ident, Set.empty)
      TPoly _ name         ->
        pure (TyNamed (Just name.loc) name.ident, Set.singleton name.ident)
      TArrow loc ty' ty''  -> do
        (rTy, setA) <- go ty'
        (rTy', setB) <- go ty''
        pure (TyFun (Just loc) rTy rTy', Set.union setA setB)
      TForall loc name ty' -> do
        (rTy', set) <- go ty'
        pure (TyForall (Just loc) name.ident rTy', Set.delete name.ident set)
      _  -> error "Not implemented yet"

checkExpr :: TyperMonad m => Expr Normal -> Ty -> m ()
checkExpr expr ty = track (InCheck expr ty) $ case (expr, ty) of
  (_, TyRef _ ty') -> checkExpr expr ty'
  (_, TyHole _ hole) | isFilled hole -> checkExpr expr (getFilled hole)
  (_, TyForall _ binder _) -> do
      ext <- existentialize ty
      scopeTy binder (checkExpr expr ext)
  (Lam _ (Raw _ (PId _ (Name _ text))) body, TyFun _ left right) -> do
      scopeVar text left (checkExpr body right)
  (expr', ty') -> do
      inferred <- inferExpr expr'
      unify inferred ty'

inferExpr :: TyperMonad m => Expr Normal -> m Ty
inferExpr expr = track (InInferExpr expr) $ do
  case expr of
    Lam loc (Raw _ (PId _ (Name loc' text))) body -> do
      argTy <- TyHole (Just loc') <$> newHole
      retTy <- scopeVar text argTy (inferExpr body)
      pure $ TyFun (Just loc) argTy retTy
    App _ fun arg -> do
      funTy <- inferExpr fun
      applyExpr funTy arg
    Var _ (Name loc text) -> lookupVar loc text
    Lit _ lit -> inferLit lit
    Ann _ expr' ty -> do
      iTy <- inferTy ty
      checkExpr expr' iTy
      pure iTy
    EHole loc _ ->
      TyHole (Just loc) <$> newHole
    ast -> error $ "Not implemented yet " ++ show ast

applyExpr :: TyperMonad m => Ty -> Expr Normal -> m Ty
applyExpr ty expr = track (InApply ty expr) $ case ty of
  TyFun _ arg ret -> checkExpr expr arg >> pure ret
  TyForall _ _ _  -> instantiate ty >>= (`applyExpr` expr)
  TyRef _ ty' -> applyExpr ty' expr
  TyHole loc hole -> do
    resHole <- readHole hole
    case resHole of
      Empty _ _ -> do
        argHole <- TyHole loc <$> newHole
        retHole <- TyHole loc <$> newHole
        fillHole hole (TyFun loc argHole retHole)
        checkExpr expr argHole
        pure retHole
      Filled t -> applyExpr t expr
  other -> typeError (NotAFunction other)
module Typer.Unify (
    unify
) where

import Typer.Env
    ( Ty(..),
      TyHole,
      Hole(Empty, Filled),
      Lvl,
      getTyPos,
      TyperMonad,
      Tracker(InUnify),
      track,
      scopeTy,
      scopeUp,
      newHole,
      instantiate,
      fillHole,
      readHole,
      setHole )
import Control.Monad (when)

import qualified Typer.Error  as Err

-- Occours and scope checking :D
unifyPreCheck :: TyperMonad m => Lvl -> TyHole -> Ty -> m ()
unifyPreCheck scope hole ty = preCheck ty
  where
    preCheck :: TyperMonad m => Ty -> m ()
    preCheck ty' = case ty of
      TyRef _      t  -> preCheck t
      TyNamed _ _     -> pure ()
      TyFun _ l r     -> preCheck l >> preCheck r
      TyForall _ _ t  -> preCheck t
      TyRigid _ _ lvl  | lvl >= scope -> Err.typeError $ (Err.EscapingScope ty')
                       | otherwise  -> pure ()
      TyHole loc hol' -> do
        when (hol' == hole) (Err.typeError $ (Err.OccoursCheck (TyHole loc hole) ty))
        resHol <- readHole hol'
        case resHol of
          Empty loc' scope' ->
            when (scope' > scope) (setHole hol' (Empty loc' scope))
          Filled t -> preCheck t

unify :: TyperMonad m => Ty -> Ty -> m ()
unify ty ty' = track (InUnify ty ty') $ case (ty, ty') of
  (TyHole _ hole, b) -> do
    resHole <- readHole hole
    case resHole of
      Empty _ _ -> instantiateLeft hole b
      Filled t -> unify t b
  (a, TyHole _ hole) -> do
    resHole <- readHole hole
    case resHole of
      Empty _ _ -> instantiateRight a hole
      Filled t -> unify a t
  (a, TyForall r binder ty'') -> do
    scopeUp $ scopeTy binder (TyNamed r binder) $ unify a ty''
  (TyForall _ _ _, a) -> do
    instTy <- instantiate ty
    unify instTy a
  (TyFun _ a b, TyFun _ a' b') -> unify a' a >> unify b b'
  (TyRef _ a, b) -> unify a b
  (a, TyRef _ b) -> unify a b
  (TyNamed _ a, TyNamed _ a')
    | a == a' -> pure ()
    | otherwise -> Err.typeError Err.CannotUnify
  (TyRigid _ _ a, TyRigid _ _ a')
    | a == a' -> pure ()
    | otherwise -> Err.typeError Err.CannotUnify
  _ -> Err.typeError Err.CannotUnify

unifyHole :: TyperMonad m => TyHole -> Ty -> m ()
unifyHole hole ty = do
  resHole <- readHole hole
  case resHole of
    Filled t -> unify t ty
    Empty _ scope -> do
      case ty of
        TyHole loc _ | ty == (TyHole loc hole) -> pure ()
        _ -> do
          unifyPreCheck scope hole ty
          fillHole hole ty

instantiateLeft :: TyperMonad m => TyHole -> Ty -> m ()
instantiateLeft hole = \case
  TyRef _ ty' -> instantiateLeft hole ty'
  TyForall r binder ty ->
    scopeTy binder (TyNamed r binder) $ instantiateLeft hole ty
  TyFun loc arg ret -> do
    argHole <- newHole
    retHole <- newHole
    fillHole hole (TyFun loc (TyHole (getTyPos arg) argHole) (TyHole (getTyPos ret) retHole))
    instantiateRight arg argHole
    instantiateLeft retHole ret
  ty -> unifyHole hole ty

instantiateRight :: TyperMonad m => Ty -> TyHole -> m ()
instantiateRight ty hole = case ty of
  TyRef _ ty' -> instantiateRight ty' hole
  ty'@(TyForall {}) -> do
    instTy <- instantiate ty'
    instantiateRight instTy hole
  TyFun loc arg ret -> do
    argHole <- newHole
    retHole <- newHole
    fillHole hole (TyFun loc (TyHole (getTyPos arg) argHole) (TyHole (getTyPos ret) retHole))
    instantiateLeft argHole arg
    instantiateRight ret retHole
  ty' -> unifyHole hole ty'
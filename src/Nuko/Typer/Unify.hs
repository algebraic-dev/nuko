module Nuko.Typer.Unify (
  unifyKind,
  unify,
  destructFun,
) where

import Nuko.Typer.Types    (TTy(..), Relation (..), derefTy, TyHole)
import Nuko.Typer.Kinds    (TKind(..), Hole(Filled, Empty), KiHole, derefKind)
import Nuko.Typer.Error    (TypeError(..))
import Nuko.Typer.Env      (MonadTyper, addLocalTy, seScope, newTyHoleWithScope, eagerInstantiate, newKindHole)
import Nuko.Utils          (terminate, flag)

import Relude              (Int, Ord ((>=), (>)), (&&), (<))
import Relude.Base         ((==))
import Relude.Bool         (when, not, Bool (..), otherwise)
import Relude.Lifted       (readIORef, writeIORef)
import Relude.Function     (($))
import Relude.Applicative  (pure, (*>))
import Lens.Micro.Platform (view)

destructFun :: MonadTyper m => TTy 'Virtual -> m (TTy 'Virtual, TTy 'Virtual)
destructFun ty = do
  instTy <- eagerInstantiate ty
  case derefTy instTy of
    TyFun a b -> pure (a, b)
    TyHole hole -> do
      content <- readIORef hole
      case content of
        Empty name scope -> do
          argTy <- newTyHoleWithScope name scope
          resTy <- newTyHoleWithScope name scope
          writeIORef hole (Filled (TyFun argTy resTy))
          pure (argTy, resTy)
        Filled f -> destructFun f
    _ -> terminate NotAFunction

unifyKind :: MonadTyper m => TKind -> TKind -> m ()
unifyKind origK origiK1 = do
    case (derefKind origK, derefKind origiK1) of
      (KiStar, KiStar) -> pure ()
      (KiHole hole, k') -> do
        content <- readIORef hole
        case content of
          Filled fil -> unifyKind fil k'
          Empty {}   -> when (not $ isEq hole k') $ do
            preCheck hole k'
            writeIORef hole (Filled k')
      (ty, KiHole hole) -> unifyKind (KiHole hole) ty
      (KiFun a b, KiFun a' b') -> unifyKind a a' *> unifyKind b b'
      (k, k') -> terminate (KindMismatch k k')
  where
    isEq hole ty = case ty of { KiHole hole' -> hole == hole'; _ -> False }

    preCheck :: MonadTyper m => KiHole -> TKind -> m ()
    preCheck hole = \case
      KiFun a b -> preCheck hole a *> preCheck hole b
      KiStar -> pure ()
      KiHole hole' -> do
        when (hole == hole') (terminate (OccoursCheckKind (KiHole hole) (KiHole hole')))
        content <- readIORef hole'
        case content of
          Empty _ _ -> pure ()
          Filled a  -> preCheck hole a

unify :: MonadTyper m => TTy 'Virtual -> TTy 'Virtual -> m ()
unify oTy oTy' = do
    case (derefTy oTy, derefTy oTy') of
      (TyErr, _) -> pure ()
      (_, TyErr) -> pure ()
      (TyHole hole, ty') -> do
        content <- readIORef hole
        case content of
          Empty _ scope -> unifyHoleTy hole scope ty'
          Filled f      -> unify f ty'
      (ty, ty'@TyHole {}) -> unify ty' ty
      (TyForall n f, TyForall _ f') -> do
        kind <- newKindHole n
        addLocalTy n kind $ do
          scope <- view seScope
          unify (f (TyVar scope))  (f' (TyVar scope))
      (TyVar a, TyVar b) | a == b -> pure ()
      (TyFun f t, TyFun f' t') -> unify f f' *> unify t t'
      (TyApp k f t, TyApp k' f' t') -> unifyKind k k' *> unify f f' *> unify t t'
      (TyIdent a, TyIdent b) | a == b -> pure ()
      (ty, ty') -> flag (Mismatch ty ty')
  where
    unifyHoleTy :: MonadTyper m => TyHole -> Int -> TTy 'Virtual -> m ()
    unifyHoleTy hole scope ty = do
      start <- view seScope
      case ty of
        TyHole hole' | hole == hole' -> pure ()
        _ -> do
          preCheck scope start hole ty
          writeIORef hole (Filled ty)

    preCheck :: MonadTyper m => Int -> Int -> TyHole -> TTy 'Virtual -> m ()
    preCheck scope start hole =
        go
      where
        go :: MonadTyper m => TTy 'Virtual -> m ()
        go uTy = do
          curScope <- view seScope
          case derefTy uTy of
            TyErr -> pure ()
            TyVar lvl
              | lvl >= scope && lvl < start -> terminate EscapingScope
              | otherwise    -> pure ()
            TyIdent _ -> pure ()
            TyApp _ l r  -> go l *> go r
            TyFun l r    -> go l *> go r
            TyForall _ t -> go (t (TyVar curScope))
            TyHole hole' -> do
              when (hole == hole') (terminate (OccoursCheck (TyHole hole) (TyHole hole')))
              resHol <- readIORef hole'
              case resHol of
                Empty loc' scope' -> when (scope' > scope) (writeIORef hole (Empty loc' scope))
                Filled t          -> go t
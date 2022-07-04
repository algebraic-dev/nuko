module Nuko.Typer.Unify (
  unify,
) where

import Nuko.Typer.Types
import Nuko.Typer.Env     (MonadTyper, TypingEnv(_teScope), addTy)
import Relude.Applicative (Applicative((*>), pure))
import Relude.Monad       (asks)
import Relude.Function    (($))
import Relude.Bool        (otherwise, when)
import Relude             ((>=), Eq ((==)), readIORef, Ord ((>)), writeIORef, Traversable (traverse), (&&))
import Nuko.Typer.Error   (ErrCause(..), TypeError(TypeError))
import Nuko.Utils         (terminate)

unifyPreCheck :: MonadTyper m => Lvl -> TyHole Virtual -> TTy Virtual -> m ()
unifyPreCheck scope hole ty = do
    preCheck ty
  where
    preCheck :: MonadTyper m => TTy Virtual -> m ()
    preCheck ty' = do
      sanitizedTy <- removeHole ty'
      curScope <- asks _teScope
      case sanitizedTy of
        TyIdent _ _ -> pure ()
        TyVar lvl
          | lvl >= scope -> terminate (TypeError $ EscapingScope ty')
          | otherwise    -> pure ()
        TyFun l r    -> preCheck l *> preCheck r
        TyForall _ t -> preCheck (t (TyVar curScope))
        TyHole hol' -> do
          when (hol' == hole) (terminate $ (TypeError $ OccoursCheck (TyHole hole) ty))
          resHol <- readIORef hol'
          case resHol of
            Empty loc' scope' -> when (scope' > scope) (writeIORef hol' (Empty loc' scope))
            Filled t          -> preCheck t

unifyHoleTy :: MonadTyper m => TyHole Virtual -> Lvl -> TTy Virtual -> m ()
unifyHoleTy hole scope ty =
  case ty of
    TyHole hole' | hole == hole' -> pure ()
    _ -> do
      unifyPreCheck scope hole ty
      writeIORef hole (Filled ty)

unify :: MonadTyper m => TTy Virtual -> TTy Virtual -> m ()
unify ty ty' = do
  tys <- traverse removeHole (ty, ty')
  case tys of
    (TyHole hole, _) -> do
      content <- readIORef hole
      case content of
        Empty _ scope -> unifyHoleTy hole scope ty'
        _             -> terminate (TypeError $ EscapingScope ty)
    (_, TyHole _) -> unify ty' ty
    (TyVar a, TyVar b) | a == b -> pure ()
    (TyFun f t, TyFun f' t') -> unify f t *> unify f' t'
    (TyIdent a a', TyIdent b b') | a == b && a' == b' -> pure ()
    (TyForall n f, TyForall _ f') -> do
      addTy n $ do
        scope <- asks _teScope
        unify (f (TyVar scope))  (f' (TyVar scope))
    _ -> terminate (TypeError $ CannotUnify)
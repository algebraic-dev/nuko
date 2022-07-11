module Nuko.Typer.Unify (
  unifyKind,
  unify
) where

import Relude.Bool         (when, Bool (..), not, otherwise)
import Relude.Base         ((==), Ord ((>), (>=)))
import Relude.Applicative  (Applicative(pure, (*>), (<*>)))
import Relude.Functor      ((<$>))
import Relude.Monad        ((=<<), gets, modify, asks, MonadReader (local))
import Relude.String       (Text)
import Relude.Lifted.IORef (writeIORef, readIORef)

import Nuko.Typer.Types
import Nuko.Typer.Env      (MonadTyper, TypingEnv(..), ScopeEnv(..), seScope)
import Nuko.Typer.Error    (TypeError(..))
import Nuko.Utils          (terminate)
import Lens.Micro.Platform  (over)
import GHC.Num             ((+), Num ((-)))
import Data.Function       (($))
import Data.Traversable    (Traversable(..))

unifyKind :: MonadTyper m => TKind -> TKind -> m ()
unifyKind k1 k1' = do
    (k, k') <- (,) <$> fixKindHoles k1 <*> fixKindHoles k1'
    case (k, k') of
      (KiHole hole, _) -> do
        content <- readIORef hole
        ty <- fixKindHoles k'
        case content of
          Filled fil -> unifyKind fil ty
          Empty {}   -> when (not $ isEq hole ty) $ do
            unifyPreCheck hole ty
            writeIORef hole (Filled ty)
      (ty, KiHole hole)        -> unifyKind (KiHole hole) ty
      (KiStar, KiStar)         -> pure ()
      (KiFun a b, KiFun a' b') -> unifyKind a a' *> unifyKind b b'
      (_, _)                   -> terminate =<< CannotUnify <$> printKind k <*> printKind k'
  where
    isEq hole ty = case ty of { KiHole hole' -> hole == hole'; _ -> False }

    unifyPreCheck :: MonadTyper m => KiHole -> TKind -> m ()
    unifyPreCheck hole = \case
      KiFun a b -> unifyPreCheck hole a *> unifyPreCheck hole b
      KiStar -> pure ()
      KiHole hole' -> do
        when (hole == hole') (terminate =<< OccoursCheckKind <$> printKind (KiHole hole'))
        content <- readIORef hole'
        case content of
          Empty _ _ -> pure ()
          Filled a  -> unifyPreCheck hole a

unify :: MonadTyper m => TTy Virtual -> TTy Virtual -> m ()
unify ty ty' = do
  tys <- traverse removeHoles (ty, ty')
  case tys of
    (TyHole hole, _) -> do
      content <- readIORef hole
      case content of
        Empty _ scope -> unifyHoleTy hole scope ty'
        Filled f      -> unify f ty'
    (_, TyHole _) -> unify ty' ty
    (TyVar a, TyVar b) | a == b -> pure ()
    (TyFun f t, TyFun f' t') -> unify f f' *> unify t t'
    (TyApp k f t, TyApp k' f' t') -> unifyKind k k' *> unify f f' *> unify t t'
    (TyIdent a, TyIdent b) | a == b -> pure ()
    (TyForall n f, TyForall _ f') -> do
      addTy n $ do
        scope <- asks _seScope
        unify (f (TyVar scope))  (f' (TyVar scope))
    _ -> do
      tyS  <- printTy [] ty
      tyS' <- printTy [] ty'
      terminate (CannotUnify tyS tyS')
  where
    removeHoles :: MonadTyper m => TTy Virtual -> m (TTy Virtual)
    removeHoles = \case
      TyHole hole -> do
        content <- readIORef hole
        case content of
          Empty {} -> pure (TyHole hole)
          Filled f -> removeHoles f
      other -> pure other

    addTy :: MonadTyper m => Text -> m a -> m a
    addTy _ act = local (over seScope (+ 1)) act

    unifyPreCheck :: MonadTyper m => Int -> TyHole -> TTy Virtual -> m ()
    unifyPreCheck scope hole = do
        preCheck
      where
        preCheck :: MonadTyper m => TTy Virtual -> m ()
        preCheck uTy = do
          sanitizedTy <- removeHoles uTy
          curScope <- asks _seScope
          case sanitizedTy of
            TyIdent _ -> pure ()
            TyVar lvl
              | lvl >= scope -> terminate EscapingScope
              | otherwise    -> pure ()
            TyApp _ l r  -> preCheck l *> preCheck r
            TyFun l r    -> preCheck l *> preCheck r
            TyForall _ t -> preCheck (t (TyVar curScope))
            TyHole hol' -> do
              when (hol' == hole) (terminate OccoursCheck)
              resHol <- readIORef hol'
              case resHol of
                Empty loc' scope' -> when (scope' > scope) (writeIORef hol' (Empty loc' scope))
                Filled t          -> preCheck t

    unifyHoleTy :: MonadTyper m => TyHole -> Int -> TTy Virtual -> m ()
    unifyHoleTy hole scope uTy =
      case ty of
        TyHole hole' | hole == hole' -> pure ()
        _ -> do
          unifyPreCheck scope hole uTy
          writeIORef hole (Filled uTy)
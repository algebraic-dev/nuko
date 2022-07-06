module Nuko.Typer.Unify (
  unifyKind
) where

import Relude.Bool        (when)
import Relude.Base        ((==))
import Relude.Applicative (Applicative(pure, (*>), (<*>)))
import Relude.Lifted      (readIORef)
import Relude.Functor     ((<$>))
import Relude.Monad       ((=<<))

import Nuko.Typer.Types   (printKind, Hole(Filled, Empty), TKind(..), KiHole)
import Nuko.Typer.Env     (MonadTyper)
import Nuko.Utils         (terminate)
import Nuko.Typer.Error   (TypeError(..))

unifyKind :: MonadTyper m => TKind -> TKind -> m ()
unifyKind k k' = case (k, k') of
    (KiHole hole, ty) -> do
      content <- readIORef hole
      case content of
        Empty {} -> unifyPreCheck hole ty
        Filled fil -> unifyKind fil ty
    (ty, KiHole hole) -> unifyKind (KiHole hole) ty
    (KiStar, KiStar) -> pure ()
    (KiFun a b, KiFun a' b') -> unifyKind a a' *> unifyKind b b'
    (_, _) -> do
      (resK, resK') <- (,) <$> printKind k <*> printKind k'
      terminate (CannotUnify resK resK')
  where
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

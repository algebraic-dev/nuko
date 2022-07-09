module Nuko.Typer.Unify (
  unifyKind
) where

import Relude.Bool        (when, Bool (..), not)
import Relude.Base        ((==))
import Relude.Applicative (Applicative(pure, (*>), (<*>)))
import Relude.Lifted      (readIORef, writeIORef, putStrLn)
import Relude.Functor     ((<$>))
import Relude.Monad       ((=<<))

import Nuko.Typer.Types   (printKind, Hole(Filled, Empty), TKind(..), KiHole, fixKindHoles)
import Nuko.Typer.Env     (MonadTyper)
import Nuko.Utils         (terminate)
import Nuko.Typer.Error   (TypeError(..))
import Data.Function      (($))

unifyKind :: MonadTyper m => TKind -> TKind -> m ()
unifyKind k k' = case (k, k') of
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

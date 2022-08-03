module Nuko.Typer.Unify (
  unifyKind,
  unify,
  destructFun,
) where

import Relude

import Control.Monad.Chronicle  (MonadChronicle (..), memento)
import Lens.Micro.Platform      (view)
import Nuko.Report.Range        (Range (..))
import Nuko.Typer.Env           (MonadTyper, addLocalTy, eagerInstantiate,
                                 endDiagnostic, newKindHole, newTyHoleWithScope,
                                 seScope)
import Nuko.Typer.Error         (TypeError (..))
import Nuko.Typer.Error.Extract (Tracker (..), extractKindMismatch,
                                 extractKindUnifierRange, extractTypeMismatch,
                                 extractTypeUnifierRange, track)
import Nuko.Typer.Kinds         (Hole (Empty, Filled), KiHole, TKind (..),
                                 derefKind)
import Nuko.Typer.Types         (Relation (..), TTy (..), TyHole, derefTy)

destructFun :: MonadTyper m => Range -> TTy 'Virtual -> m (TTy 'Virtual, TTy 'Virtual)
destructFun range ty = do
  instTy <- eagerInstantiate (derefTy ty)
  case instTy of
    TyErr     -> pure (TyErr, TyErr)
    TyFun a b -> pure (a, b)
    TyHole hole -> do
      content <- readIORef hole
      case content of
        Empty name scope -> do
          argTy <- newTyHoleWithScope name scope
          resTy <- newTyHoleWithScope name scope
          writeIORef hole (Filled (TyFun argTy resTy))
          pure (argTy, resTy)
        Filled f -> destructFun range f
    t -> endDiagnostic (NotAFunction range t) range

unifyKind :: MonadTyper m => Range -> TKind -> TKind -> m ()
unifyKind range t' ty' = track (InKindUnify range t' ty') $ go t' ty'
  where
    isEq hole ty = case ty of { KiHole hole' -> hole == hole'; _ -> False }

    go :: MonadTyper m => TKind -> TKind -> m ()
    go origK origiK1 = do
        case (derefKind origK, derefKind origiK1) of
          (KiStar, KiStar) -> pure ()
          (KiHole hole, k') -> do
            content <- readIORef hole
            case content of
              Filled fil -> go fil k'
              Empty {}   -> unless (isEq hole k') $ do
                preCheck hole k'
                writeIORef hole (Filled k')
          (ty, KiHole hole) -> go (KiHole hole) ty
          (KiFun a b, KiFun a' b') -> go a a' *> go b b'
          _ -> extractKindMismatch

    preCheck :: MonadTyper m => KiHole -> TKind -> m ()
    preCheck hole = \case
      KiFun a b -> preCheck hole a *> preCheck hole b
      KiStar -> pure ()
      KiHole hole' -> do
        when (hole == hole') $ do
          range' <- extractKindUnifierRange
          endDiagnostic (OccoursCheckKind range' (KiHole hole) (KiHole hole')) range'
        content <- readIORef hole'
        case content of
          Empty _ _ -> pure ()
          Filled a  -> preCheck hole a

unify :: MonadTyper m => Range -> TTy 'Virtual -> TTy 'Virtual -> m (TTy 'Virtual)
unify range got expect = do
    res <- memento $ track (InUnify range got expect) $ go got expect
    case res of
      Left err -> dictate err *> pure TyErr
      Right () -> pure expect
  where
    go :: MonadTyper m => TTy 'Virtual -> TTy 'Virtual -> m ()
    go oTy oTy' = track (InUnify range oTy oTy') $ do
      case (derefTy oTy, derefTy oTy') of
        (TyHole hole, ty') -> do
          content <- readIORef hole
          case content of
            Empty _ scope -> unifyHoleTy hole scope ty'
            Filled f      -> go f ty'
        (ty, ty'@TyHole {}) -> go ty' ty
        (TyErr, _) -> pure ()
        (_, TyErr) -> pure ()
        (TyForall n f, TyForall _ f') -> do
          kind <- newKindHole n
          addLocalTy n kind $ do
            scope <- view seScope
            go (f (TyVar scope))  (f' (TyVar scope))
        (TyVar a, TyVar b) | a == b -> pure ()
        (TyFun f t, TyFun f' t') -> go f f' *> go t t'
        (TyApp k f t, TyApp k' f' t') -> unifyKind range k k' *> go f f' *> go t t'
        (TyIdent a, TyIdent b) | a == b -> pure ()
        (_, _) -> extractTypeMismatch

    unifyHoleTy :: MonadTyper m => TyHole -> Int -> TTy 'Virtual -> m ()
    unifyHoleTy hole scope ty = do
      start' <- view seScope
      case ty of
        TyHole hole' | hole == hole' -> pure ()
        _ -> do
          preCheck scope start' hole ty
          writeIORef hole (Filled ty)

    preCheck :: MonadTyper m => Int -> Int -> TyHole -> TTy 'Virtual -> m ()
    preCheck scope initial hole =
        go'
      where
        go' :: MonadTyper m => TTy 'Virtual -> m ()
        go' uTy = do
          curScope <- view seScope
          case derefTy uTy of
            TyErr -> pure ()
            TyVar lvl
              | lvl >= scope && lvl < initial -> do
                range' <- extractTypeUnifierRange
                endDiagnostic (EscapingScope range') range'
              | otherwise    -> pure ()
            TyIdent _ -> pure ()
            TyApp _ l r  -> go' l *> go' r
            TyFun l r    -> go' l *> go' r
            TyForall _ t -> go' (t (TyVar curScope))
            TyHole hole' -> do
              when (hole == hole') $ do
                range' <- extractTypeUnifierRange
                endDiagnostic (OccoursCheck range' (TyHole hole) (TyHole hole')) range'
              resHol <- readIORef hole'
              case resHol of
                Empty loc' scope' -> when (scope' > scope) (writeIORef hole (Empty loc' scope))
                Filled t          -> go' t

module Nuko.Typer.Infer.Pat (
  inferPat
) where

import Relude.String            (Text)
import Relude.Functor           ((<$>))
import Relude.Container         (HashMap)
import Relude.Applicative       (pure)
import Relude.Monad             (Maybe(..))
import Relude.Foldable          (Foldable(length))
import Relude.Base              (Eq((/=)))
import Relude.Bool              (when)
import Relude.Lifted            (readIORef, newIORef, writeIORef)

import Nuko.Typer.Tree          ()
import Nuko.Typer.Unify         (unify, unifyKind)
import Nuko.Typer.Infer.Literal (inferLit)
import Nuko.Typer.Infer.Type    (inferTy)
import Nuko.Typer.Error         (TypeError(..))
import Nuko.Typer.Env           (MonadTyper, newTyHole, getTy, tsConstructors)
import Nuko.Typer.Types         (TTy (..), Virtual, Hole (..), TKind (KiStar))
import Nuko.Resolver.Tree       (ReId(ReId))
import Nuko.Tree.Expr           (Pat(..))
import Nuko.Tree                (Re, Tc)
import Nuko.Utils               (terminate)

import Control.Monad.State      (StateT, MonadTrans (lift), MonadState (get), modify, foldM)

import qualified Control.Monad.State as State
import qualified Data.HashMap.Strict as HashMap

inferPat :: MonadTyper m => Pat Re -> m (Pat Tc, HashMap Text (TTy Virtual), TTy Virtual)
inferPat pat = do
    ((pat', ty), t) <- State.runStateT (go pat) HashMap.empty
    pure (pat', t, ty)
  where
    applyPat :: MonadTyper m => TTy Virtual -> Pat Re -> StateT (HashMap Text (TTy Virtual)) m (Pat Tc, TTy Virtual)
    applyPat ty pat' = case ty of
      TyFun   f t -> do
        (patRes, inferedTy) <- go pat'
        lift (unify inferedTy f)
        pure (patRes, t)
      TyHole hole -> do
        content <- lift (readIORef hole)
        case content of
          Empty n scope -> do
            tyArg <- TyHole <$> lift (newIORef (Empty n scope))
            tyRes <- TyHole <$> lift (newIORef (Empty n scope))
            writeIORef hole (Filled (TyFun tyArg tyRes))
            (patRes, inferedTy) <- go pat
            lift (unify inferedTy tyArg)
            pure (patRes, inferedTy)
          Filled _ -> lift (terminate NotAFunction)
      _ -> lift (terminate NotAFunction)

    go :: MonadTyper m => Pat Re -> StateT (HashMap Text (TTy Virtual)) m (Pat Tc, TTy Virtual)
    go = \case
      PWild ext               -> lift ((PWild ext, ) <$> newTyHole "_")
      PId (ReId name loc) ext -> do
        env <- get
        case HashMap.lookup name env of
          Just _   -> lift (terminate (NameResolution name))
          Nothing  -> do
            hole <- lift (newTyHole name)
            modify (HashMap.insert name hole)
            pure (PId (ReId name loc) ext, hole)
      PCons path arg e -> do
        (constType, argSize) <- lift (getTy tsConstructors path)
        when (argSize /= length arg) (terminate (ExpectedConst argSize (length arg)))
        (pats, resType) <- foldM (\(pats, t) pat' -> (\(patR, tR) -> (patR : pats, tR)) <$> applyPat t pat') ([], constType) arg
        pure (PCons path pats e, resType)
      PLit lit e       -> do
        (resLit, resTy) <- lift (inferLit lit)
        pure (PLit resLit e, resTy)
      PAnn pat' tty _   -> do
        (resTy, resKind) <- lift (inferTy [] tty)
        lift (unifyKind resKind KiStar)
        (resPat, resTy') <- go pat'
        lift (unify resTy resTy')
        pure (resPat, resTy)

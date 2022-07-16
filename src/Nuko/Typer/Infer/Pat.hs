module Nuko.Typer.Infer.Pat (
  inferPat,
) where

import Relude                   (($))
import Relude.Base              (Eq(..))
import Relude.Applicative       (pure)
import Relude.Monad             (Maybe(..), modify)
import Relude.Container         (HashMap)

import Control.Monad.Reader     (foldM, MonadTrans(lift))
import Lens.Micro.Platform      (use, at)
import Data.List                (length)
import Control.Monad            (when)

import Nuko.Resolver.Tree       ()
import Nuko.Typer.Tree          ()
import Nuko.Typer.Infer.Literal (inferLit)
import Nuko.Typer.Infer.Type    (inferTy)
import Nuko.Typer.Error         (TypeError(..))
import Nuko.Typer.Unify         (unify, destructFun)
import Nuko.Typer.Types         (TTy(..), Relation(..))
import Nuko.Typer.Env           (genTyHole, getTy, newTyHole, tsConstructors, DataConsInfo(_parameters, _constructorTy), MonadTyper, qualifyPath)
import Nuko.Tree.Expr           (Pat(..))
import Nuko.Names               (coerceTo, genIdent, mkName, Attribute(Untouched), Label(Label), Name, NameKind(TyName), ValName)
import Nuko.Utils               (terminate)
import Nuko.Tree                (Re, Tc)

import qualified Control.Monad.State as State
import qualified Data.HashMap.Strict as HashMap

type InferPat m a = State.StateT (HashMap (Name ValName) (TTy 'Virtual)) m a

inferPat :: MonadTyper m => Pat Re -> m ((Pat Tc, TTy 'Virtual), HashMap (Name ValName) (TTy 'Virtual))
inferPat pat =
    State.runStateT (go pat) HashMap.empty
  where
    newId :: MonadTyper m => Name ValName -> InferPat m (Name ValName, TTy 'Virtual)
    newId name = do
      resTy <- use (at name)
      case resTy of
        Just _  -> terminate (NameResolution (Label name))
        Nothing -> do
          resHole <- lift $ newTyHole (coerceTo TyName name)
          modify (HashMap.insert name resHole)
          pure (name, resHole)

    applyPat ::  MonadTyper m => ([Pat Tc], TTy 'Virtual) -> Pat Re -> InferPat m ([Pat Tc], TTy 'Virtual)
    applyPat (args, fnTy) arg = do
      (argTy, retTy) <- lift $ destructFun fnTy
      (argRes, argTy') <- go arg
      lift $ unify argTy argTy'
      pure (argRes : args, retTy)

    go :: MonadTyper m => Pat Re -> InferPat m (Pat Tc, TTy 'Virtual)
    go = \case
      PWild ext -> do
        tyHole <- lift (newTyHole (mkName TyName (genIdent "_") Untouched))
        pure (PWild ext, tyHole)
      PId place ext -> do
        (name, resTy) <- newId place
        pure (PId name ext, resTy)
      PCons path args ext -> do
        qualified <- lift (qualifyPath path)
        constInfo <- lift (getTy tsConstructors qualified)
        when (constInfo._parameters /= length args) $ terminate (ExpectedConst constInfo._parameters (length args))
        (argsRes, resTy) <- foldM applyPat ([], constInfo._constructorTy) args
        pure (PCons path argsRes ext, resTy)
      PLit lit ext -> do
        (resLit, resTy) <- lift $ inferLit lit
        pure (PLit resLit ext, resTy)
      PAnn pat' ty ext -> do
        (resPat, resPatTy) <- go pat'
        (resTy, _) <- lift $ inferTy [] ty
        lift $ unify resPatTy resTy
        pure (PAnn resPat resTy ext, resTy)
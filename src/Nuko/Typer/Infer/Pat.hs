module Nuko.Typer.Infer.Pat (
  inferPat,
  checkPat
) where

import Relude                   (($))
import Relude.Base              (Eq(..))
import Relude.Applicative       (pure)
import Relude.Monad             (Maybe(..), modify)
import Relude.Container         (HashMap)

import Control.Monad.Reader     (foldM, MonadTrans(lift))
import Lens.Micro.Platform      (use, at, view)
import Data.List                (length)
import Control.Monad            (when)

import Nuko.Resolver.Tree       ()
import Nuko.Typer.Tree          ()

import Nuko.Typer.Infer.Literal (inferLit)
import Nuko.Typer.Infer.Type    (inferClosedTy)
import Nuko.Typer.Error         (TypeError(..))
import Nuko.Typer.Unify         (unify, destructFun)
import Nuko.Typer.Types         (TTy(..), Relation(..), quote, evaluate)
import Nuko.Typer.Env           (getTy, newTyHole, tsConstructors, DataConsInfo(_parameters), MonadTyper, qualifyPath, endDiagnostic, seScope, eagerInstantiate)
import Nuko.Tree.Expr           (Pat(..))
import Nuko.Names               (coerceTo, genIdent, mkName, Attribute(Untouched), Name, NameKind(TyName), ValName)
import Nuko.Tree                (Re, Tc)

import qualified Control.Monad.State as State
import qualified Data.HashMap.Strict as HashMap
import Nuko.Report.Range (Range, HasPosition (getPos))

type InferPat m a = State.StateT (HashMap (Name ValName) (TTy 'Virtual)) m a

checkPat :: MonadTyper m => Pat Re -> TTy 'Virtual -> m (HashMap (Name ValName) (TTy 'Virtual))
checkPat pat ty = case (pat, ty) of
  (_, TyForall _ f) -> do
    ctxLvl <- view seScope
    checkPat pat (f (TyVar ctxLvl))
  _ -> do
    ((resPat, inferedTy), bindings) <- inferPat pat
    instTy <- eagerInstantiate inferedTy
    unify (getPos resPat) instTy ty
    pure bindings

inferPat :: MonadTyper m => Pat Re -> m ((Pat Tc, TTy 'Virtual), HashMap (Name ValName) (TTy 'Virtual))
inferPat pat =
    State.runStateT (go pat) HashMap.empty
  where
    newId :: MonadTyper m => Name ValName -> InferPat m (Name ValName, TTy 'Virtual)
    newId name = do
      resTy <- use (at name)
      case resTy of
        Just ty' -> pure (name, ty')
        Nothing  -> do
          resHole <- lift $ newTyHole (coerceTo TyName name)
          modify (HashMap.insert name resHole)
          pure (name, resHole)

    applyPat ::  MonadTyper m => Range -> ([Pat Tc], TTy 'Virtual) -> Pat Re -> InferPat m ([Pat Tc], TTy 'Virtual)
    applyPat range (args, fnTy) arg = do
      (argTy, retTy) <- lift $ destructFun range fnTy
      (argRes, argTy') <- go arg
      lift $ unify range argTy argTy'
      pure (argRes : args, retTy)

    go :: MonadTyper m => Pat Re -> InferPat m (Pat Tc, TTy 'Virtual)
    go = \case
      PWild ext -> do
        tyHole <- lift (newTyHole (mkName TyName (genIdent "_") Untouched))
        pure (PWild (quote 0 tyHole, ext), tyHole)
      PId place _ -> do
        (name, resTy) <- newId place
        pure (PId name (quote 0 resTy), resTy)
      PCons path args ext -> do
        qualified <- lift (qualifyPath path)
        (constRealTy, constInfo) <- lift (getTy tsConstructors qualified)
        when (constInfo._parameters /= length args) $
          lift (endDiagnostic (ExpectedConst (getPos path) constInfo._parameters (length args)) ext)
        let constTy = evaluate [] constRealTy
        let fnRange = getPos path
        (argsRes, resTy) <- foldM (applyPat fnRange) ([], constTy) args
        pure (PCons path argsRes (quote 0 resTy, ext), resTy)
      PLit lit ext -> do
        (resLit, resTy) <- lift $ inferLit lit
        pure (PLit resLit ext, resTy)
      PAnn pat' ty ext -> do
        (resPat, resPatTy) <- go pat'
        (resTy, _) <- lift $ inferClosedTy ty
        lift $ unify (getPos resPat) resPatTy resTy
        pure (PAnn resPat (quote 0 resTy) (quote 0 resTy, ext), resTy)
      POr pat' pat'' ext -> do
        (resPat, resPatTy) <- go pat'
        (resPat', resPatTy') <- go pat''
        lift $ unify (getPos resPat) resPatTy resPatTy'
        pure (POr resPat resPat' (quote 0 resPatTy, ext), resPatTy)
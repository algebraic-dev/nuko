module Nuko.Typer.Infer.Expr (
  inferExpr,
  checkExpr,
) where

import Relude                   (($), (<$>), One (one), fst, (.))
import Relude.Monad             (Maybe(..), (=<<))
import Relude.Functor           (Bifunctor(first))
import Relude.List.NonEmpty     (NonEmpty((:|)))
import Relude.Applicative       (pure)

import Control.Monad.Reader     (foldM)
import Lens.Micro.Platform      (view)
import Data.Traversable         (for)
import Data.List.NonEmpty       ((<|))

import Nuko.Typer.Env
import Nuko.Typer.Tree          ()
import Nuko.Typer.Infer.Literal (inferLit, boolTy)
import Nuko.Typer.Infer.Type    (inferClosedTy)
import Nuko.Typer.Infer.Pat     (inferPat)
import Nuko.Typer.Error         (TypeError(..))
import Nuko.Typer.Unify         (unify, destructFun)
import Nuko.Typer.Types         (TTy(..), Relation(..), quote, derefTy, evaluate)

import Nuko.Resolver.Tree       ()
import Nuko.Tree.Expr           (Expr(..))
import Nuko.Names               (Name, ValName, Path (..))
import Nuko.Tree                (Re, Tc, Block (..), Var (..))

import qualified Data.HashMap.Strict as HashMap
import Nuko.Report.Range (getPos)

inferBlock :: MonadTyper m => Block Re -> m (Block Tc, TTy 'Virtual)
inferBlock = \case
  BlBind expr rest -> do
    (resExpr,       _) <- inferExpr expr
    (resBlock, resTy') <- inferBlock rest
    pure (BlBind resExpr resBlock, resTy')
  BlVar var rest   -> do
    ((patRes, patTy), bindings) <- inferPat var.pat
    exprRes <- checkExpr var.val patTy
    (resBlock, resTy') <- addLocals bindings (inferBlock rest)
    pure (BlVar (Var patRes exprRes var.ext) resBlock, resTy')
  BlEnd expr       -> do
    (resExpr, resTy) <- runInst $ inferExpr expr
    pure (BlEnd resExpr, resTy)

checkExpr :: MonadTyper m => Expr Re -> TTy 'Virtual -> m (Expr Tc)
checkExpr expr tty = case (expr, tty) of
  (_, TyForall _ f) -> do
    ctxLvl <- view seScope
    checkExpr expr (f (TyVar ctxLvl))
  (Lam pat expr' e, TyFun argTy retTy) -> do
    ((patRes, patTy), bindings) <- inferPat pat
    unify patTy argTy
    exprRes <- addLocals bindings (checkExpr expr' retTy)
    pure (Lam patRes exprRes (quote 0 (TyFun argTy retTy), e))
  _ -> do
    (resExpr, inferedTy) <- inferExpr expr
    instTy <- eagerInstantiate inferedTy
    unify instTy tty
    pure resExpr

applyExpr :: MonadTyper m => TTy 'Virtual -> Expr Re -> m (Expr Tc, TTy 'Virtual)
applyExpr fnTy expr = do
  (argTy, resTy) <- destructFun fnTy
  argRes <- checkExpr expr argTy
  pure (argRes, resTy)

accApplyExpr :: MonadTyper m => (NonEmpty (Expr Tc), TTy 'Virtual) -> Expr Re -> m (NonEmpty (Expr Tc), TTy 'Virtual)
accApplyExpr (exprs, fnTy) expr = first (<| exprs) <$> applyExpr fnTy expr

inferExpr :: MonadTyper m => Expr Re -> m (Expr Tc, TTy 'Virtual)
inferExpr = \case
  Lit lit ext -> do
    (resLit, resTy) <- inferLit lit
    pure (Lit resLit ext, resTy)
  Lower path _ -> do
    ty <- case path of
      Local _ path' -> getLocal path'
      Full _ qual   -> (evaluate [] . fst) <$> getTy tsVars qual
    pure (Lower path (quote 0 ty), ty)
  Upper path _ -> do
    qualified <- qualifyPath path
    dataInfo <- getTy tsConstructors qualified
    pure (Upper path dataInfo._constructorTy, evaluate [] dataInfo._constructorTy)
  Ann exp ty ext -> do
    (resTy, _) <- inferClosedTy ty
    exprRes <- checkExpr exp resTy
    pure (Ann exprRes (quote 0 resTy) ext, resTy)
  Block block ext -> do
    (blockRes, resTy) <- inferBlock block
    pure (Block blockRes (quote 0 resTy, ext), resTy)
  If con if' else' ext -> do
    (conRes, conTy) <- inferExpr con
    unify conTy boolTy
    (ifRes, ifTy)  <- runInst $ inferExpr if'
    elseRes <- checkExpr else' ifTy
    pure (If conRes ifRes elseRes (quote 0 ifTy, ext), ifTy)
  Match scrut cas ext -> do
    (scrutRes, scrutTy) <- inferExpr scrut
    resTy <- genTyHole
    casesRes <- for cas $ \(pat, expr) ->  do
      ((resPat, patTy), bindings) <- inferPat pat
      resExpr <- addLocals bindings (checkExpr expr resTy)
      unify scrutTy patTy
      pure (resPat, resExpr)
    let resDeref = derefTy resTy
    pure (Match scrutRes casesRes (quote 0 resDeref, ext), derefTy resTy)
  Lam pat expr ext -> do
    ((resPat, patTy), bindings) <- inferPat pat
    (resExpr, exprTy) <- runInst $ addLocals bindings (inferExpr expr)
    pure (Lam resPat resExpr (quote 0 (TyFun patTy exprTy), ext), TyFun patTy exprTy)
  App expr (x :| xs) ext -> do
    (fnExpr, fnTy) <- inferExpr expr
    (argExpr, fnResTy) <- applyExpr fnTy x
    (resArgs, resTy) <- foldM accApplyExpr (one argExpr, fnResTy) xs
    pure (App fnExpr resArgs (quote 0 resTy, ext), resTy)
  Field expr field ext -> do
    (resExpr, resTy) <- inferExpr expr
    (argFieldTy, resFieldTy) <- destructFun =<< getFieldByTy field resTy
    unify argFieldTy resTy
    pure (Field resExpr field (quote 0 resFieldTy, ext), resFieldTy)

getFieldByTy :: MonadTyper m => Name ValName -> TTy 'Virtual -> m (TTy 'Virtual)
getFieldByTy field = \case
  TyApp _ f _ -> getFieldByTy field f
  TyIdent p   -> do
    res <- getTy tsTypeFields p
    case HashMap.lookup field res of
      Just res' -> pure (evaluate [] res'._fiResultType)
      Nothing   -> terminateLocalized (CannotInferField) (Just $ getPos field)
  _ -> terminateLocalized (CannotInferField) (Just $ getPos field)
module Nuko.Typer.Infer.Expr (
  inferExpr,
  checkExpr,
) where

import Relude

import Nuko.Names               (Name, Path (..), ValName)
import Nuko.Report.Range        (Range (..), getPos)
import Nuko.Resolver.Tree       ()
import Nuko.Tree                (Block (..), Re, Tc, Var (..))
import Nuko.Tree.Expr           (Expr (..))
import Nuko.Typer.Env
import Nuko.Typer.Error         (TypeError (..))
import Nuko.Typer.Infer.Literal (boolTy, inferLit)
import Nuko.Typer.Infer.Pat     (inferPat)
import Nuko.Typer.Infer.Type    (inferClosedTy)
import Nuko.Typer.Match         (checkPatterns)
import Nuko.Typer.Tree          ()
import Nuko.Typer.Types         (Relation (..), TTy (..), derefTy, evaluate,
                                 quote)
import Nuko.Typer.Unify         (destructFun, unify)

import Control.Monad.Reader     (foldM)
import Data.List.NonEmpty       ((<|))
import Data.Traversable         (for)
import Lens.Micro.Platform      (view)

import Data.HashMap.Strict      qualified as HashMap

inferBlock :: MonadTyper m => Block Re -> m (Block Tc, TTy 'Virtual)
inferBlock = \case
  BlBind expr rest -> do
    (resExpr,       _) <- inferExpr expr
    (resBlock, resTy') <- inferBlock rest
    pure (BlBind resExpr resBlock, resTy')
  BlVar var rest   -> do
    ((patRes, patTy), bindings) <- inferPat var.pat
    exprRes <- checkExpr var.val patTy
    checkPatterns (getPos var) (one patRes)
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
    unify (getPos pat) patTy argTy
    checkPatterns (getPos pat) (one patRes)
    exprRes <- addLocals bindings (checkExpr expr' retTy)
    pure (Lam patRes exprRes (quote 0 (TyFun argTy retTy), e))
  _ -> do
    (resExpr, inferedTy) <- inferExpr expr
    instTy <- eagerInstantiate inferedTy
    unify (getPos expr) instTy tty
    pure resExpr

applyExpr :: MonadTyper m => Range -> TTy 'Virtual -> Expr Re -> m (Expr Tc, TTy 'Virtual)
applyExpr range fnTy expr = do
  (argTy, resTy) <- destructFun range fnTy
  argRes <- checkExpr expr argTy
  pure (argRes, resTy)

accApplyExpr :: MonadTyper m => Range -> (NonEmpty (Expr Tc), TTy 'Virtual) -> Expr Re -> m (NonEmpty (Expr Tc), TTy 'Virtual)
accApplyExpr range (exprs, fnTy) expr = first (<| exprs) <$> applyExpr range fnTy expr

inferExpr :: MonadTyper m => Expr Re -> m (Expr Tc, TTy 'Virtual)
inferExpr = \case
  Lit lit extension -> do
    (resLit, resTy) <- inferLit lit
    pure (Lit resLit extension, resTy)
  Lower path _ -> do
    ty <- case path of
      Local _ path' -> getLocal path'
      Full _ qual   -> evaluate [] . fst <$> getTy tsVars qual
    pure (Lower path (quote 0 ty), ty)
  Upper path _ -> do
    qualified <- qualifyPath path
    (consTy, _) <- getTy tsConstructors qualified
    pure (Upper path consTy, evaluate [] consTy)
  Ann expr ty extension -> do
    (resTy, _) <- inferClosedTy ty
    exprRes <- checkExpr expr resTy
    pure (Ann exprRes (quote 0 resTy) extension, resTy)
  Block block extension -> do
    (blockRes, resTy) <- inferBlock block
    pure (Block blockRes (quote 0 resTy, extension), resTy)
  If con if' else' extension -> do
    (conRes, conTy) <- inferExpr con
    unify (getPos con) conTy boolTy
    (ifRes, ifTy)  <- runInst $ inferExpr if'
    elseRes <- checkExpr else' ifTy
    pure (If conRes ifRes elseRes (quote 0 ifTy, extension), ifTy)
  Match scrut cas range -> do
    (scrutRes, nonInstScrutTy) <- inferExpr scrut
    scrutTy <- eagerInstantiate nonInstScrutTy
    resTy <- genTyHole
    casesRes <- for cas $ \(pat, expr) ->  do
      ((resPat, patTy), bindings) <- inferPat pat
      unify (getPos resPat) patTy scrutTy
      resExpr <- addLocals bindings (checkExpr expr resTy)
      pure (resPat, resExpr)
    checkPatterns range (fst <$> casesRes)
    let resDeref = derefTy resTy
    pure (Match scrutRes casesRes (quote 0 resDeref, range), derefTy resTy)
  Lam pat expr extension -> do
    ((resPat, patTy), bindings) <- inferPat pat
    checkPatterns (getPos pat) (one (resPat))
    (resExpr, exprTy) <- runInst $ addLocals bindings (inferExpr expr)
    pure (Lam resPat resExpr (quote 0 (TyFun patTy exprTy), extension), TyFun patTy exprTy)
  App expr (x :| xs) extension -> do
    (fnExpr, fnTy) <- inferExpr expr
    let fnRange = getPos expr
    (argExpr, fnResTy) <- applyExpr fnRange fnTy x
    (resArgs, resTy) <- foldM (accApplyExpr fnRange) (one argExpr, fnResTy) xs
    pure (App fnExpr resArgs (quote 0 resTy, extension), resTy)
  Field expr field extension -> do
    -- TODO: Probably will have problems with eager instantiation
    (resExpr, resTy) <- inferExpr expr
    (argFieldTy, resFieldTy) <- destructFun (getPos field) =<< getFieldByTy field resTy
    unify (getPos expr) argFieldTy resTy
    pure (Field resExpr field (quote 0 resFieldTy, extension), resFieldTy)

-- TODO: If the type is private and it's not in the current namespace, we have to thrown
-- an error. It would cause problems in the hot reloading.
getFieldByTy :: MonadTyper m => Name ValName -> TTy 'Virtual -> m (TTy 'Virtual)
getFieldByTy field = \case
  TyApp _ f _ -> getFieldByTy field f
  TyIdent p   -> do
    res <- getTy tsTypeFields p
    case HashMap.lookup field res of
      Just res' -> pure (evaluate [] res'._fiResultType)
      Nothing   -> endDiagnostic (CannotInferField (getPos field)) (getPos field)
  _ -> endDiagnostic (CannotInferField (getPos field)) (getPos field)

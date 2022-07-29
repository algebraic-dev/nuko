module Nuko.Typer.Infer.Expr (
  inferExpr,
  checkExpr,
) where

import Relude                   (($), (<$>), One (one), fst, (.), Foldable (toList), putTextLn, show, Semigroup ((<>)))
import Relude.Monad             (Maybe(..), (=<<))
import Relude.Functor           (Bifunctor(first))
import Relude.List.NonEmpty     (NonEmpty((:|)))
import Relude.Applicative       (pure)

import Control.Monad.Reader     (foldM, foldM_)
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
import Nuko.Report.Range        (getPos, Range (..))
import Nuko.Typer.Match         (checkUseful, addRow, matrixFromColumn, toMatchPat, isExhaustive)
import Pretty.Format            (Format(format))

import qualified Data.HashMap.Strict as HashMap

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
    unify (getPos pat) patTy argTy
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
  Ann exp ty extension -> do
    (resTy, _) <- inferClosedTy ty
    exprRes <- checkExpr exp resTy
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
  Match scrut cas extension -> do
    (scrutRes, scrutTy) <- inferExpr scrut
    resTy <- genTyHole
    casesRes <- for cas $ \(pat, expr) ->  do
      ((resPat, patTy), bindings) <- inferPat pat
      resExpr <- addLocals bindings (checkExpr expr resTy)
      unify (getPos scrut) scrutTy patTy
      pure (resPat, resExpr)
    let resDeref = derefTy resTy

    -- Exhaustiveness
    let pats = toList $ fst <$> casesRes
    case pats of
      (x : xs) -> do
        foldM_ (\matrix pat -> do
              res <- checkUseful matrix pat
              putTextLn $ "Resultado:" <> show res <> " | " <> format (toMatchPat pat)
              pure (addRow matrix pat)) (matrixFromColumn [x]) xs
      _ -> pure ()

    exhaustive <- isExhaustive pats
    putTextLn $ format exhaustive
    --when (not exhaustive) $ endDiagnostic NotExhaustive (Just (getPos (scrut)))

    pure (Match scrutRes casesRes (quote 0 resDeref, extension), derefTy resTy)
  Lam pat expr extension -> do
    ((resPat, patTy), bindings) <- inferPat pat
    (resExpr, exprTy) <- runInst $ addLocals bindings (inferExpr expr)
    pure (Lam resPat resExpr (quote 0 (TyFun patTy exprTy), extension), TyFun patTy exprTy)
  App expr (x :| xs) extension -> do
    (fnExpr, fnTy) <- inferExpr expr
    let fnRange = getPos expr
    (argExpr, fnResTy) <- applyExpr fnRange fnTy x
    (resArgs, resTy) <- foldM (accApplyExpr fnRange) (one argExpr, fnResTy) xs
    pure (App fnExpr resArgs (quote 0 resTy, extension), resTy)
  Field expr field extension -> do
    (resExpr, resTy) <- inferExpr expr
    (argFieldTy, resFieldTy) <- destructFun (getPos field) =<< getFieldByTy field resTy
    unify (getPos expr) argFieldTy resTy
    pure (Field resExpr field (quote 0 resFieldTy, extension), resFieldTy)

getFieldByTy :: MonadTyper m => Name ValName -> TTy 'Virtual -> m (TTy 'Virtual)
getFieldByTy field = \case
  TyApp _ f _ -> getFieldByTy field f
  TyIdent p   -> do
    res <- getTy tsTypeFields p
    case HashMap.lookup field res of
      Just res' -> pure (evaluate [] res'._fiResultType)
      Nothing   -> endDiagnostic (CannotInferField (getPos field)) (getPos field)
  _ -> endDiagnostic (CannotInferField (getPos field)) (getPos field)
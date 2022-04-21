module Typer.Infer (
    inferLit,
    inferTy,
    inferExpr,
    processTyDecls,
    processLetDecl,
    preprocessLetDecl,
    processProgram
) where

import Typer.Env
import Expr hiding (Name)
import Syntax.Range           (getPos, Range)
import Syntax.Tree            (Normal)
import Typer.Unify            (unify)
import Typer.Types            (isFilled, getFilled)
import Data.Set               (Set)
import Typer.Error            (typeError, ErrCause (NotAFunction, CannotFindType))
import Control.Monad          (when)
import Data.Foldable          (traverse_)

import qualified Expr                as Exp
import qualified Data.Set            as Set
import qualified Data.Map            as Map
import qualified Typer.Error         as Err
import qualified Typer.Types         as Typer
import qualified Control.Monad.State as State

lookupVar :: TyperMonad m => Range -> Typer.Env.Name -> m Ty
lookupVar loc' name = do
  res <- State.gets (Map.lookup name . variables)
  maybe (typeError (Err.CannotFind loc' name))
        (\ty -> pure $ maybe (TyRef (Just loc') ty) (const ty) $ getTyPos ty)
        res

lookupCons :: TyperMonad m => Range -> Typer.Env.Name -> m Ty
lookupCons loc' name = do
  res <- State.gets (Map.lookup name . dataCons)
  maybe (typeError (Err.CannotFind loc' name))
        (\ty -> pure $ maybe (TyRef (Just loc') ty) (const ty) $ getTyPos ty)
        res

getTy :: TyperMonad m => Range -> Typer.Name -> m Ty
getTy range name = do
  result <- State.gets (Map.lookup name . types)
  maybe
    (typeError (CannotFindType range name))
    (pure . TyRef (Just range))
    result

-- Programs

processProgram :: TyperMonad  m => Program Normal -> m ()
processProgram program = do
  processTyDecls program.progType
  processLetDecls program.progLet

-- Let decleration

processSeq :: TyperMonad m => [(Pattern Normal, Typer Normal)] -> (Map.Map Name Ty) -> m ([Ty], Map.Map Name Ty)
processSeq [] ids = pure ([], ids)
processSeq ((pat, ty) : xs) ids = do
  (patTy, ids') <- inferPattern pat ids
  tyTy  <- inferTy ty
  unify patTy tyTy
  (resTys, ids'') <- processSeq xs ids'
  pure (tyTy : resTys, ids'')

preprocessLetDecl :: TyperMonad m => LetDecl Normal -> m ()
preprocessLetDecl (LetDecl name args ret _ range) = track (InInferGen range) $ do
  retTy       <- inferTy ret
  (argsTy, _) <- processSeq args Map.empty
  generalized <- generalize (foldr (TyFun Nothing) retTy argsTy)
  addTy name.ident generalized

processLetDecl :: TyperMonad m => LetDecl Normal -> m ()
processLetDecl (LetDecl _ args ret body range) = track (InInferGen range) $ do
    retTy        <- inferTy ret
    (_, newVars) <- processSeq args Map.empty
    scopeVars (Map.toList newVars) $ 
      checkExpr body retTy
    pure ()

processLetDecls :: TyperMonad m => [LetDecl Normal] -> m ()
processLetDecls decls = do
  traverse_ preprocessLetDecl decls
  traverse_ processLetDecl decls

-- Start environment with the crazy things

processDataCons :: TyperMonad m => TypeDecl Normal -> m ()
processDataCons (TypeDecl name _ cons _) =
    go cons
  where
    processSumField :: TyperMonad m => (Exp.Name Normal, [Typer Normal]) -> m ()
    processSumField (consName, args) = do
      processedArgs <- traverse inferTy args
      resTy <- getTy name.loc name.ident
      addCons consName.ident (foldr (TyFun Nothing) resTy processedArgs)

    processProdField :: TyperMonad m => (Exp.Name Normal, Typer Normal) -> m ()
    processProdField (_, _) = pure $ error "Not implemented record types yet"

    go :: TyperMonad m => TypeCons Normal -> m ()
    go = \case
      TcSum _ cons'     -> traverse_ processSumField cons'
      TcRecord _ fields -> traverse_ processProdField fields
      TcSyn _      ty'  -> inferTy ty' >>= addTy name.ident

processTyDecls :: TyperMonad m => [TypeDecl Normal] -> m ()
processTyDecls decls = do
  traverse_ (\name -> addTy name (TyNamed Nothing name)) (map (ident . typName) decls)
  traverse_
    (\decl -> when ((length decl.typArgs) > 0) (error $ "Sorry, i cannot process args for types by now" ++ show decl))
    decls
  traverse_ processDataCons decls

-- Infer expressions

seqPattern :: TyperMonad m => [Pattern Normal] -> (Map.Map Name Ty) -> m ([Ty], Map.Map Name Ty)
seqPattern [] ids = pure ([], ids)
seqPattern (x : xs) ids = do
  (resTy, ids') <- inferPattern x ids
  (resTys, ids'') <- seqPattern xs ids'
  pure (resTy : resTys, ids'')

checkPattern :: TyperMonad m => Pattern Normal -> Ty -> (Map.Map Name Ty) -> m (Map.Map Name Ty)

checkPattern pat@(PId _ name) ty ids = track (InCheckPat pat) $ do
  pure (Map.insert name.ident ty ids)

checkPattern pat ty ids = track (InCheckPat pat) $ do
  (inferredPat, ids') <- inferPattern pat ids
  unify inferredPat ty
  pure ids'

inferPattern :: TyperMonad m => Pattern Normal -> (Map.Map Name Ty) -> m (Ty, Map.Map Name Ty)
inferPattern pat ids = track (InInferPat pat) $ case pat of
  PWild range -> do
    hole <- TyHole (Just range) <$> newHole
    pure (hole, ids)
  PCons range name patterns -> do
    consTy <- lookupCons name.loc name.ident
    retTy <- TyHole (Just range) <$> newHole
    (patTys, ids') <- seqPattern patterns ids
    unify (foldr (TyFun Nothing) retTy patTys) consTy
    pure (retTy, ids')
  PId _ name -> do
    idTy <- TyHole (Just name.loc) <$> newHole
    pure (idTy, Map.insert name.ident idTy ids)
  PLit _ lit -> do
    resTy <- inferLit lit
    pure (resTy, ids)
  PAnn _ pat' ty -> do
    inferredTy <- inferTy ty
    ids' <- checkPattern pat' inferredTy ids
    pure (inferredTy, ids')

inferLit :: TyperMonad m => Literal Normal -> m Ty
inferLit lit = track (InInferLit lit) $ case lit of
  LString loc' _ -> named loc' "String"
  LChar loc'   _ -> named loc' "Char"
  LInt loc'    _ -> named loc' "Int"
  LDouble loc' _ -> named loc' "Double"

inferTy :: TyperMonad m => Typer Normal -> m Ty
inferTy t = track (InInferTy t) $ do
    (ty, unbounded) <- go t
    pure (foldl (flip $ TyForall (Just (getPos t))) ty unbounded)
  where
    go :: TyperMonad m => Typer Normal -> m (Ty, Set Name)
    go ty = track (InInferTy ty) $ case ty of
      TSimple _ name       -> do
        ty' <- getTy name.loc name.ident
        pure (ty', Set.empty)
      TPoly _ name         ->
        pure (TyNamed (Just name.loc) name.ident, Set.singleton name.ident)
      TArrow loc' ty' ty''  -> do
        (rTy, setA) <- go ty'
        (rTy', setB) <- go ty''
        pure (TyFun (Just loc') rTy rTy', Set.union setA setB)
      TForall loc' name ty' -> do
        (rTy', set) <- go ty'
        pure (TyForall (Just loc') name.ident rTy', Set.delete name.ident set)
      _  -> error "Not implemented yet"

unifyFun :: TyperMonad m => Ty -> m (Ty, Ty)
unifyFun (TyFun _ a b) = pure (a,b)
unifyFun tau = do
  argTy <- TyHole Nothing <$> newHole
  retTy <- TyHole Nothing <$> newHole
  unify tau (TyFun Nothing argTy retTy)
  pure (argTy, retTy)

checkExpr :: TyperMonad m => Expr Normal -> Ty -> m ()
checkExpr expr ty = track (InCheck expr ty) $ case (expr, ty) of
  (_, TyRef _ ty') -> checkExpr expr ty'
  (_, TyHole _ hole) | isFilled hole -> checkExpr expr (getFilled hole)
  (_, TyForall r binder _) -> do
    ext <- existentialize ty
    scopeUp $ scopeTy binder (TyNamed r binder) (checkExpr expr ext)
  (Lam _ param body, tau) -> do
    (argTy, retTy) <- unifyFun tau
    vars <- checkPattern param argTy Map.empty
    scopeVars (Map.toList vars) (checkExpr body retTy)
  (expr', ty') -> do
    inferred <- inferExpr expr'
    unify inferred ty'

inferSttms :: TyperMonad m => Sttms Normal -> m Ty
inferSttms (End expr) = inferExpr expr
inferSttms (SExpr expr next) = do
  _ <- inferExpr expr
  inferSttms next

inferSttms (SAssign assign next) = do
  bodyTy <- inferExpr assign.assignVal
  maybe
    (pure ())
    (\ty -> inferTy ty >>= unify bodyTy)
    assign.assignRet
  (argTy, newVars) <- inferPattern assign.assignName Map.empty
  unify argTy bodyTy
  traversedMap <- traverse generalize newVars
  scopeVars (Map.toList traversedMap)
    (inferSttms next)

inferExpr :: TyperMonad m => Expr Normal -> m Ty
inferExpr expr = track (InInferExpr expr) $ do
  case expr of
    Lam loc' pat body -> do
      (argTy, vars) <- inferPattern pat Map.empty
      retTy <- scopeVars (Map.toList vars) (inferExpr body)
      pure $ TyFun (Just loc') argTy retTy
    App _ fun arg -> do
      funTy <- inferExpr fun
      applyExpr funTy arg
    Var _ (Exp.Name loc' text) -> do
      res <- lookupVar loc' text
      pure res
    Cons _ (Exp.Name loc' text) -> lookupCons loc' text
    Lit _ lit -> inferLit lit
    Ann _ expr' ty -> do
      iTy <- inferTy ty
      checkExpr expr' iTy
      pure iTy
    EHole loc' _ ->
      TyHole (Just loc') <$> newHole
    Block _ sttms -> inferSttms sttms
    ast -> error $ "Not implemented yet " ++ show ast

applyExpr :: TyperMonad m => Ty -> Expr Normal -> m Ty
applyExpr ty expr = track (InApply ty expr) $ case ty of
  TyFun _ arg ret -> checkExpr expr arg >> pure ret
  TyForall _ _ _  -> instantiate ty >>= (`applyExpr` expr)
  TyRef _ ty' -> applyExpr ty' expr
  TyHole loc' hole -> do
    resHole <- readHole hole
    case resHole of
      Empty _ _ -> do
        argHole <- TyHole loc' <$> newHole
        retHole <- TyHole loc' <$> newHole
        fillHole hole (TyFun loc' argHole retHole)
        checkExpr expr argHole
        pure retHole
      Filled t -> applyExpr t expr
  other -> typeError (NotAFunction other)
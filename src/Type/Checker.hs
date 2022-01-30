module Type.Checker where

import Type.Types      (RType(..), TypeKind(..), TypeId)
import Type.Context     (Context, CtxElem(..))
import Syntax.Parser.Ast (Normal)
import Syntax.Expr 

import qualified Data.Text           as Text
import qualified Type.Types          as Ty
import qualified Type.Context        as Ctx
import qualified Control.Monad.State as State
import Control.Monad (replicateM)

type Checker = State.MonadState [String]

fromAstType :: Type Normal -> RType 'Poly 
fromAstType = \case  
  TSimple xs na    -> undefined
  TPoly xp na      -> undefined
  TArrow xa ty ty' -> undefined
  TCons xc na tys  -> undefined
  TForall xf na ty -> undefined
  TExt xe          -> undefined

freshName :: Checker m => m String
freshName = State.state $ \case
   [] -> error "There's no fresh name for it!"
   s : ss -> (s, ss)

checkWF :: Checker m => Context -> RType 'Poly -> m ()
checkWF ctx ty =
  if Ctx.typeWellFormed ctx ty
    then pure ()
    else error $ "The type is not well formed: " ++ show ty

subType :: Checker m => Context -> RType 'Poly -> RType 'Poly -> m Context
subType ctx ty ty' = do
  checkWF ctx ty
  checkWF ctx ty'
  case (ty, ty') of
    (TyUnit, TyUnit) -> pure ctx
    (TyAlpha a, TyAlpha b) | a == b -> pure ctx
    (TyExists a, TyExists b) | a == b -> pure ctx
    (TyFun a b, TyFun a' b') -> do
      delta <- subType ctx a' a
      subType delta (Ctx.applyContext delta b) (Ctx.applyContext delta b')
    (a, TyForall b fTy) -> do
      new <- freshName
      ctx' <- subType (CtxAlpha new : ctx) a (Ty.substitute b (TyAlpha new) fTy)
      pure $ Ctx.dropMarker ctx' (CtxAlpha new)
    (TyForall b fTy, a) -> do
      new <- freshName
      ctx' <- subType (CtxExists new : CtxMarker new : ctx) (Ty.substitute b (TyExists new) fTy) a
      pure $ Ctx.dropMarker ctx' (CtxMarker new)
    (TyExists a, b) | a `notElem` Ty.freeVars b -> instantiateL ctx a b
    (b, TyExists a) | a `notElem` Ty.freeVars b -> instantiateR ctx b a
    (a, b) -> error $ show a ++ " is not a subtype (idk) of " ++ show b

instantiateL :: Checker m => Context -> TypeId -> RType 'Poly -> m Context
instantiateL ctx ex = \case
  TyUnit -> pure $ Ctx.solve ctx ex TyUnit
  TyAlpha s -> pure $ Ctx.solve ctx ex (TyAlpha s)
  TyExists b | Ctx.isOrdered ctx ex b -> pure $ Ctx.solve ctx b (TyExists ex)
             | otherwise -> pure $ Ctx.solve ctx ex (TyExists b)
  TyFun ty ty' -> do
    (a,b) <- (,) <$> freshName <*> freshName
    let newCtx = Ctx.insertAt ctx (CtxExists ex)
                  [ CtxSolved ex (TyFun (TyExists a) (TyExists b))
                  , CtxExists b
                  , CtxExists a]
    theta <- instantiateR newCtx ty a
    instantiateL theta b (Ctx.applyContext theta ty')
  TyForall s ty -> do
    b <- freshName
    ctx' <- instantiateL (CtxAlpha b : ctx) ex (Ty.substitute s (TyAlpha b) ty)
    pure $ Ctx.dropMarker ctx' (CtxAlpha b)

instantiateR :: Checker m => Context -> RType 'Poly ->  TypeId -> m Context
instantiateR ctx typ ex = case typ of
  TyUnit -> pure $ Ctx.solve ctx ex TyUnit
  TyAlpha s -> pure $ Ctx.solve ctx ex (TyAlpha s)
  TyExists b | Ctx.isOrdered ctx ex b -> pure $ Ctx.solve ctx b (TyExists ex)
             | otherwise -> pure $ Ctx.solve ctx ex (TyExists b)
  TyFun ty ty' -> do
    (a,b) <- (,) <$> freshName <*> freshName
    let newCtx = Ctx.insertAt ctx (CtxExists ex)
                  [ CtxSolved ex (TyFun (TyExists a) (TyExists b))
                  , CtxExists b
                  , CtxExists a]
    theta <- instantiateL newCtx a ty
    instantiateR theta (Ctx.applyContext theta ty') b
  TyForall s ty -> do
    b <- freshName
    ctx' <- instantiateR (CtxExists b : CtxMarker b : ctx) (Ty.substitute s (TyExists b) ty) ex
    pure $ Ctx.dropMarker ctx' (CtxMarker b)

exprTypeCheck :: Checker m => Context -> Expr Normal -> RType 'Poly ->  m Context
exprTypeCheck ctx expr ty = case (expr, ty) of 
  (Lam _ (Raw _ (PId _ (Name (_, id')))) ex, TyFun a b) -> do
    let var = CtxVar (Text.unpack id') a 
    ctxRes <- exprTypeCheck (var : ctx) ex b
    pure $ Ctx.dropMarker ctxRes var
  (e, TyForall b ty') -> do 
    (`Ctx.dropMarker` CtxAlpha b) <$> exprTypeCheck (CtxAlpha b : ctx) e ty'
  (expr', _) -> do 
    (b, ctx') <- exprTypeSynth ctx expr'
    subType ctx' (Ctx.applyContext ctx' ty) (Ctx.applyContext ctx' b)  

litTypeSynth :: Literal Normal -> RType 'Poly
litTypeSynth = \case
  LChar _ _     -> TyAlpha "Char"
  LString _ _   -> TyAlpha "String"
  LInt _ _      -> TyAlpha "Int"
  LDouble _ _   -> TyAlpha "Double"

exprTypeSynth :: Checker m => Context -> Expr Normal -> m (RType 'Poly, Context)
exprTypeSynth ctx = \case
  Lit _ lit            -> pure (litTypeSynth lit, ctx) -- 1I⇒
  Var _ (Name (_, i)) -> case Ctx.findVar (Text.unpack i) ctx of -- Var
    Nothing -> error $ "Cannot find type for var: " ++ Text.unpack i
    Just ty -> pure (ty, ctx)
  Lam _ (Raw _ (PId _ (Name (_, id')))) ex -> do -- →I⇒
    (a,b) <- (,) <$> freshName <*> freshName
    let var = CtxVar (Text.unpack id') (TyExists a)
    let ctx' = var : CtxExists a : CtxExists b : CtxMarker a : ctx
    delta <- exprTypeCheck ctx' ex (TyExists b)
    let (l, r) = Ctx.breakMarker delta (CtxMarker a)
    let tau = Ctx.applyContext l (TyFun (TyExists a) (TyExists b))
    let oldVars = Ctx.unsolved l
    vars <- traverse (const freshName) oldVars 
    let subs = foldl (flip $ uncurry Ty.substitute) tau (zip oldVars (TyAlpha <$> vars))
    pure (foldl (flip TyForall) subs vars, r)
  App _ ex ex' -> do 
    (ty, ctx') <- exprTypeSynth ctx ex
    typeApplySynth ctx' (Ctx.applyContext ctx' ty) ex' 
  _ -> error "Cannot process this kind of thing now!"

typeApplySynth :: Checker m => Context -> RType 'Poly -> Expr Normal -> m (RType 'Poly, Context)
typeApplySynth ctx ty expr = case (ty, expr) of 
  (TyExists alpha, e) -> do 
    (a,b) <- (,) <$> freshName <*> freshName
    let ctx' = Ctx.insertAt ctx (CtxExists alpha) [CtxExists b, CtxExists a, CtxSolved alpha (TyFun (TyExists a) (TyExists b))]
    resCtx <- exprTypeCheck ctx' e (TyExists a)
    pure (TyExists b, resCtx)
  (TyFun a b, e) -> do 
    ctx' <- exprTypeCheck ctx e a 
    pure (b, ctx')
  (TyForall b t, e) -> do 
    alpha <- freshName
    let newTy = Ty.substitute b (TyExists alpha) t
    typeApplySynth (CtxExists alpha : ctx) newTy e
  _ -> error "Wot"

names :: [[Char]]
names = [1..] >>= flip replicateM ['a'..'z']

runGen :: State.State [String] a -> a 
runGen op = State.evalState op (map ('\'' :) names)
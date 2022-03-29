module Typer.Infer where

import Typer.Context
import Typer.Errors
import Typer.Types

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map (Map)
import qualified Data.Map as Map
import Syntax.Bounds
import Data.Text (Text)
import GHC.IO (throwIO)
import Control.Monad (when)
import qualified Typer.Kinds as Kinds
import Syntax.Expr
import Syntax.Parser.Ast (Normal)

type HoleRef = IORef (Hole (Type 'Eval))

type HoleMap = Map String HoleRef

upLvl :: Ctx -> Ctx 
upLvl ctx = ctx { ctxTypeLvl = ctxTypeLvl ctx + 1 }

newHole :: Ctx -> IO (Type 'Eval)
newHole ctx = do
  ref <- newIORef (Empty (ctxTypeLvl ctx))
  pure (TyHole ref)

newScopedHole :: Int -> IO (Type 'Eval)
newScopedHole scope = do
  ref <- newIORef (Empty scope)
  pure (TyHole ref)

holeWhen :: (Lvl -> IO a) -> (Type 'Eval -> IO a) -> HoleRef -> IO a
holeWhen onEmpty onFilled hole = do
  res <- readIORef hole
  case res of
    Empty n   -> onEmpty n
    Filled ty -> onFilled ty

findType :: Ctx -> Bounds -> Text -> IO (Type 'Eval)
findType Ctx { ctxVars = vars } pos key = case Map.lookup key vars of
  Nothing -> throwIO (CantFindVar pos key)
  Just ki -> pure ki

subst :: Text -> Type 'Eval -> Type 'Eval -> IO (Type 'Eval)
subst from to = \case
  TyBounded s -> pure (TyBounded s)
  TyNamed txt     | txt == from -> pure to
                  | otherwise   -> pure $ TyNamed txt
  TyForall txt ty | txt == from -> pure $ TyForall txt ty
                  | otherwise   -> TyForall txt <$> subst from to ty
  TyLoc bo ty     -> TyLoc bo <$> subst from to ty
  TyFun ty ty'    -> TyFun    <$> subst from to ty <*> subst from to ty'
  TyApp ki ty ty' -> TyApp ki <$> subst from to ty <*> subst from to ty'
  TyHole hole -> holeWhen (const $ pure (TyHole hole)) (subst from to) hole

-- Polymorphic subtypinega

preCheck :: Ctx -> HoleRef -> Lvl -> Type 'Eval -> IO ()
preCheck ctx hole scope = \case
  TyBounded s ->  when (s >= scope) (error "Escaping it's scope")
  TyNamed _   -> pure ()
  TyFun ty ty'   -> preCheck ctx hole scope ty >> preCheck ctx hole scope ty'
  TyApp _ ty ty' -> preCheck ctx hole scope ty >> preCheck ctx hole scope ty'
  TyForall _ ty  -> preCheck ctx hole scope ty
  TyHole ir -> do
    when (ir == hole) (error "Infinite type lol")
    holeWhen (\scope' -> when (scope' > scope) (writeIORef ir (Empty scope)))
             (preCheck ctx hole scope)
             ir
  TyLoc _ ty -> preCheck ctx hole scope ty


-- Γ ⊢ ^α :=< A ⊣ ∆
instantiateL :: Ctx -> HoleRef -> Lvl -> Type 'Eval -> IO ()
instantiateL ctx hole scope' = \case
  TyForall _ body -> instantiateL ctx hole scope' body
  TyFun a b -> do -- InstLArr
    (TyHole holeA) <- newScopedHole scope'
    (TyHole holeB) <- newScopedHole scope'
    writeIORef hole (Filled (TyFun (TyHole holeA) (TyHole holeB)))
    instantiateR ctx a scope' holeA
    instantiateL ctx holeB scope' b
  ty -> holeWhen (\scope -> do when (ty /= TyHole hole)
                                    (preCheck ctx hole scope ty
                                  >> writeIORef hole (Filled ty)))
                 (\filled -> subType ctx filled ty)
                 hole

-- Γ ⊢ A =<: ^α ⊣ ∆
instantiateR :: Ctx -> Type 'Eval -> Lvl -> HoleRef -> IO ()
instantiateR ctx t scope' hole = do 
  case t of
    TyForall binder body -> do
      hole' <- newHole ctx
      instantiated <- subst binder hole' body
      instantiateL ctx hole scope' instantiated
    TyFun a b -> do -- InstLArr
      (TyHole holeA) <- newScopedHole scope'
      (TyHole holeB) <- newScopedHole scope'
      writeIORef hole (Filled (TyFun (TyHole holeA) (TyHole holeB)))
      instantiateL ctx holeA scope' a
      instantiateR ctx b scope' holeB
    ty -> holeWhen (\scope -> when (ty /= TyHole hole)
                                  (preCheck ctx hole scope ty
                                >> writeIORef hole (Filled ty)))
                  (\filled -> subType ctx filled ty)
                  hole

subType :: Ctx -> Type 'Eval -> Type 'Eval -> IO ()
subType ctx t t' = case (t, t') of
  (TyLoc _ a, b) -> subType ctx a b
  (a, TyLoc _ b) -> subType ctx a b
  (TyNamed a, TyNamed b)     | a == b -> pure ()
  (TyBounded a, TyBounded b) | a == b -> pure ()
  (TyHole hole, _) ->
    holeWhen (\scope  -> instantiateL ctx hole scope t')
             (\holeTy -> subType ctx holeTy t') hole
  (_, TyHole hole) ->
    holeWhen (\scope  -> instantiateR ctx t scope hole)
             (subType ctx t) hole
  (TyForall binder body, _) -> do
    hole <- newHole ctx
    bodySubst <- subst binder hole body
    subType ctx bodySubst t'
  (_, TyForall _ body) -> do
    subType (upLvl ctx) body t
  (TyFun a b, TyFun a' b')  ->
    subType ctx a a' >> subType ctx b b'
  (TyApp k a b, TyApp k' a' b') -> do
    Kinds.unify k k'
    subType ctx a a'
    subType ctx b b'
  _ -> throwIO (CantUnifyType t t')

-- Inferencia

inferExpr :: Ctx -> Expr Normal -> IO (Type 'Eval)
inferExpr ctx = \case
  Ann _ expr ty -> do 
    (inferTy, _) <- Kinds.infer ctx ty
    putStrLn $ "Ann: (" ++ show inferTy ++ ")  " ++ show expr
    checkExpr ctx expr inferTy
    pure inferTy
  Lam _ (Raw _ (PId _ (Name _ name))) body -> do
    argTy <- newHole ctx
    resTy <- inferExpr (addTy ctx name argTy) body
    pure $ TyFun argTy resTy
  App _ f arg -> do
    fTy <- inferExpr ctx f
    applyExpr ctx fTy arg
  Var _ (Name bounds text) -> findType ctx bounds text
  _ -> undefined

checkExpr :: Ctx -> Expr Normal -> Type 'Eval -> IO ()
checkExpr ctx expr t = case (expr, t) of
  (_, TyLoc _ a) -> checkExpr ctx expr a
  (_, TyHole hole) -> holeWhen (const sub) (checkExpr ctx expr) hole 
  (_, TyForall binder body) -> do 
    instTy <- subst binder (TyBounded (ctxTypeLvl ctx)) body
    checkExpr (upLvl ctx) expr instTy
  (Lam _ (Raw _ (PId _ (Name _ name))) body, TyFun a b) -> 
    checkExpr (addTy ctx name a) body b 
  _ -> sub
  where 
    sub = do 
      infTy <- inferExpr ctx expr 
      subType ctx infTy t  

applyExpr :: Ctx -> Type 'Eval -> Expr Normal -> IO (Type 'Eval)
applyExpr ctx t expr = case t of 
    TyLoc _ a -> applyExpr ctx a expr
    TyFun ty ty' -> checkExpr ctx expr ty >> pure ty'
    TyForall txt ty -> do
      hole <- newHole ctx 
      instTy <- subst txt hole ty
      applyExpr ctx instTy expr
    TyHole ir -> holeWhen (whenEmpty ir) (\res -> applyExpr ctx res expr) ir
    _ -> error "Not a function"
  where 
    whenEmpty hole scope = do 
      holeA <- newScopedHole scope 
      holeB <- newScopedHole scope
      writeIORef hole (Filled $ TyFun holeA holeB)
      checkExpr ctx expr holeA
      pure holeB
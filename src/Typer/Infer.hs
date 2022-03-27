module Typer.Infer where

import Typer.Context
import Typer.Errors
import Typer.Types

import Syntax.Expr
import Syntax.Parser.Ast (Normal)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map (Map)
import qualified Data.Map as Map
import Syntax.Bounds
import Data.Text (Text)
import GHC.IO (throwIO)
import Control.Monad (when)

type HoleRef = IORef (Hole (Type 'Eval))

type HoleMap = Map String HoleRef

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
  TyNamed txt     | txt == from -> pure to
                  | otherwise   -> pure $ TyNamed txt
  TyForall txt ty | txt == from -> pure $ TyForall txt ty
                  | otherwise   -> TyForall txt <$> subst from to ty
  TyLoc bo ty -> TyLoc bo <$> subst from to ty
  TyBounded n -> pure $ TyBounded n
  TyFun ty ty' -> TyFun <$> subst from to ty <*> subst from to ty'
  TyApp ki ty ty' -> TyApp ki <$> subst from to ty <*> subst from to ty'
  TyHole hole -> holeWhen (const $ pure (TyHole hole)) (subst from to) hole

-- Polymorphic subtyping

preCheck :: Ctx -> HoleRef -> Lvl -> Type 'Eval -> IO ()
preCheck ctx hole scope = \case
  TyNamed _   -> pure ()
  TyBounded scope' -> when (scope' >= scope) (error "Lol using variable outside of the scope")
  TyFun ty ty' -> preCheck ctx hole scope ty >> preCheck ctx hole scope ty'
  TyApp _ ty ty' -> preCheck ctx hole scope ty >> preCheck ctx hole scope ty'
  TyForall _ ty -> preCheck ctx hole scope ty
  TyHole ir -> do
    when (ir == hole) (error "Infinite type lol")
    holeWhen (\scope' -> when (scope' > scope) (writeIORef ir (Empty scope)))
             (preCheck ctx hole scope)
             ir
  TyLoc _ ty -> preCheck ctx hole scope ty


-- Γ ⊢ ^α :=< A ⊣ ∆
subTypeHoleTy :: Ctx -> HoleRef -> Lvl -> Type 'Eval -> IO ()
subTypeHoleTy ctx hole scope' = \case
  TyForall _ body -> subTypeHoleTy ctx hole scope' body
  TyFun a b -> do -- InstLArr
    (TyHole holeA) <- newScopedHole scope'
    (TyHole holeB) <- newScopedHole scope'
    writeIORef hole (Filled (TyFun (TyHole holeA) (TyHole holeB)))
    subTypeTyHole ctx a scope' holeA
    subTypeHoleTy ctx holeB scope' b
  ty -> holeWhen (\scope -> do when (ty /= TyHole hole)
                                    (preCheck ctx hole scope ty
                                  >> writeIORef hole (Filled ty)))
                 (\filled -> subType ctx filled ty)
                 hole

-- Γ ⊢ A =<: ^α ⊣ ∆
subTypeTyHole :: Ctx -> Type 'Eval -> Lvl -> HoleRef -> IO ()
subTypeTyHole ctx t scope' hole = do 
  case t of
    TyForall binder body -> do
      hole' <- newHole ctx
      instantiated <- subst binder hole' body
      subTypeHoleTy ctx hole scope' instantiated
    TyFun a b -> do -- InstLArr
      (TyHole holeA) <- newScopedHole scope'
      (TyHole holeB) <- newScopedHole scope'
      writeIORef hole (Filled (TyFun (TyHole holeA) (TyHole holeB)))
      subTypeHoleTy ctx holeA scope' a
      subTypeTyHole ctx b scope' holeB
    ty -> holeWhen (\scope -> when (ty /= TyHole hole)
                                  (preCheck ctx hole scope ty
                                >> writeIORef hole (Filled ty)))
                  (\filled -> subType ctx filled ty)
                  hole

subType :: Ctx -> Type 'Eval -> Type 'Eval -> IO ()
subType ctx t t' = case (t, t') of
  (TyNamed a, TyNamed b)     | a == b -> pure ()
  (TyBounded a, TyBounded b) | a == b -> pure ()
  (TyHole hole, _) ->
    holeWhen (\scope  -> subTypeHoleTy ctx hole scope t')
             (\holeTy -> subType ctx holeTy t') hole
  (_, TyHole hole) ->
    holeWhen (\scope  -> subTypeTyHole ctx t scope hole)
             (subType ctx t) hole
  (TyForall binder body, _) -> do
    hole <- newHole ctx
    bodySubst <- subst binder hole body
    subType ctx bodySubst t'
  (_, TyForall _ body) -> do
    subType ctx body t
  (TyFun a b, TyFun a' b')  ->
    subType ctx a a' >> subType ctx b b'
  _ -> throwIO (CantUnifyType t t')
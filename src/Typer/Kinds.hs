module Typer.Kinds where

import Data.Map (Map)
import Data.Text (Text)
import GHC.IO (unsafePerformIO)
import Data.Data (Typeable)
import Data.IORef (IORef, readIORef, newIORef, writeIORef)
import Syntax.Parser.Ast (Normal)
import Control.Exception (Exception, throwIO, catch)

import qualified Data.Map    as Map
import qualified Syntax.Expr as AST
import qualified Data.Text   as Text

data EvalStatus = Pure | Eval

type Lvl = Int

-- Helpers

bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f ma mb = do
  a <- ma
  b <- mb
  f a b

-- Exceptions

data TypeError
  = CantUnify (Kind 'Eval) (Kind 'Eval)
  | OccoursCheck (Kind 'Eval) (Kind 'Eval)
  | CantFindVar Text
  | CantFindKind Text
  deriving (Show, Typeable)

instance Exception TypeError where

-- Defs

data Hole a
  = Empty Lvl
  | Filled a

data Kind :: EvalStatus -> * where
  Star :: Kind s
  KFun  :: Kind s -> Kind s -> Kind s
  KHole :: IORef (Hole (Kind 'Eval)) -> Kind 'Eval

data Type :: EvalStatus -> * where
  TyNamed  :: Text -> Type s
  TyFun    :: Type s -> Type s -> Type s
  TyApp    :: Kind s -> Type s -> Type s -> Type s
  TyForall :: Text   -> Type s -> Type s
  --TyHole   :: IORef (Hole (Type 'Eval)) -> Type 'Eval

-- Show instances

instance Show (Type a) where
  show = \case
    TyNamed s -> Text.unpack s
    TyFun t@TyForall {} t'  -> concat [ "(", show t, ") -> ", show t' ]
    TyFun ty@TyFun {} ty'   -> concat [ "(", show ty, ") -> ", show ty' ]
    TyFun ty ty'            -> concat [ show ty, " -> ", show ty' ]
    TyApp _ ty ty'          -> concat [ show ty, " ", show ty' ]
    TyForall n ty           -> concat [ "∀", Text.unpack n, ".", show ty ]

instance Show (Kind a) where
  show = \case
    Star -> "*"
    KFun ki@KFun {} ki' -> concat ["(", show ki, ") -> ", show ki']
    KFun ki ki' -> concat [show ki, " -> ", show ki']
    KHole ir ->
     case unsafePerformIO (readIORef ir) of
       Empty lvl -> show lvl
       Filled ki -> show ki

data Ctx
  = Ctx { ctxTypeLvl :: Lvl
        , ctxKindLvl :: Lvl
        , ctxVars    :: Map Text (Type 'Eval)
        , ctxTypes   :: Map Text (Kind 'Eval)
        , ctxNames   :: Map Text Int -- Usefull for mapping names
        }

upKindLvl :: Ctx -> Ctx
upKindLvl ctx = ctx { ctxKindLvl = ctxKindLvl ctx + 1 }

addTy :: Ctx -> Text -> Kind 'Eval -> Ctx
addTy ctx k v = ctx { ctxTypes = Map.insert k v (ctxTypes ctx) }

newHole :: Ctx -> IO (Kind 'Eval)
newHole ctx = do
  ref <- newIORef (Empty (ctxKindLvl ctx))
  pure (KHole ref)

findType :: Ctx -> Text -> IO (Type 'Eval)
findType Ctx { ctxVars = vars } key = case Map.lookup key vars of
  Nothing -> throwIO (CantFindVar key)
  Just ki -> pure ki

findKind :: Ctx -> Text -> IO (Kind 'Eval)
findKind Ctx { ctxTypes = types } key = case Map.lookup key types of
  Nothing -> throwIO (CantFindKind key)
  Just ki -> pure ki

unifyHoles :: IORef (Hole (Kind 'Eval)) -> IORef (Hole (Kind 'Eval)) -> IO ()
unifyHoles h h' = do
  valueH  <- readIORef h
  valueH' <- readIORef h'
  case (valueH, valueH') of
    (Filled a, _) -> unifyHole h' a
    (_, Filled a) -> unifyHole h' a
    (Empty lvlH, Empty lvlH') ->
      if lvlH > lvlH'
        then writeIORef h valueH'
        else writeIORef h' valueH
  
unifyHole :: IORef (Hole (Kind 'Eval)) -> Kind 'Eval -> IO ()
unifyHole hole kind = do
    holeInt <- readIORef hole
    case holeInt of
      Filled t -> unify t kind
      Empty  lvl -> do
        catch (occoursCheck lvl kind) $ \case
          OccoursCheck _ _ -> throwIO (OccoursCheck (KHole hole) kind)
          other            -> throwIO other
        writeIORef hole (Filled kind)
  where
    occoursCheck :: Lvl -> Kind 'Eval -> IO ()
    occoursCheck lvl = \case
      Star -> pure ()
      KFun ki ki' -> occoursCheck lvl ki >> occoursCheck lvl ki'
      KHole hole' -> do
        if hole == hole'
          then throwIO (OccoursCheck (KHole hole) (KHole hole'))
          else pure ()
        resHole' <- readIORef hole'
        case resHole' of
          Empty lvlHole' ->
            if lvlHole' > lvl
              then writeIORef hole' (Empty lvl)
              else pure ()
          Filled ki -> occoursCheck lvl ki

unify :: Kind 'Eval -> Kind 'Eval -> IO ()
unify t t' = do
  
  putStrLn $ concat ["Unify: ", show t, " and ", show t']
  case (t, t') of
    (Star, Star)           -> pure ()
    (KHole h, KHole h') | h == h' -> pure ()
    (KFun a b, KFun a' b') -> unify a a' >> unify b b'
    (KHole h, KHole h')    -> unifyHoles h h'
    (b', KHole h)          -> unifyHole h b'
    (KHole h, b')          -> unifyHole h b'
    _                      -> throwIO (CantUnify t t')

check :: Ctx -> AST.Typer Normal -> Kind 'Eval -> IO (Type 'Eval)
check ctx ty kind = do
  putStrLn $ concat ["Checking: ", show ty, " and ", show kind]
  (ty', kind') <- infer ctx ty
  unify kind kind'
  pure ty'

infer :: Ctx -> AST.Typer Normal -> IO (Type 'Eval, Kind 'Eval)
infer ctx t = do
  putStrLn $ "Infer: " ++ show t
  case t of
    AST.TSimple _ na    -> do
      kind <- findKind ctx (AST.getId na)
      pure (TyNamed (AST.getId na), kind)
    AST.TPoly _ na      -> do
      kind <- findKind ctx (AST.getId na)
      pure (TyNamed (AST.getId na), kind)
    AST.TArrow _ ty ty' -> do
      tyA <- check ctx ty Star
      tyB <- check ctx ty' Star
      pure (TyFun tyA tyB, Star)
    AST.TApp _ fTy argTy'   -> do
      hA <- newHole ctx
      hB <- newHole ctx
      tyR  <- check ctx fTy (KFun hA hB)
      tyR' <- check ctx argTy' hA
      pure (TyApp hB tyR tyR', hB)
    AST.TForall _ na ty -> do
      h <- newHole ctx
      (t', k) <- infer (upKindLvl $ addTy ctx (AST.getId na) h) ty
      pure (TyForall (AST.getId na) t', KFun h k)
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Typer.Kinds where

import Data.Text   (Text)
import Data.IntMap (IntMap)
import Data.IORef  (IORef, readIORef, newIORef, writeIORef)
import Control.Exception (throwIO)
import GHC.IO (catch)

import Syntax.Parser.Ast (Normal)
import Syntax.Bounds     (Bounds)

import Typer.Types
import Typer.Context
import Typer.Errors

import qualified Data.Map    as Map
import qualified Data.IntMap as IMap
import qualified Syntax.Expr as Expr

type HoleRef = IORef (Hole (Kind 'Eval))

newHole :: Ctx -> IO (Kind 'Eval)
newHole ctx = do
  ref <- newIORef (Empty (ctxKindLvl ctx))
  pure (KHole ref)

-- | Tries to transform a kind scheme to a normal eval kind 
instantiate :: Ctx -> KindScheme -> IO (Kind 'Eval)
instantiate ctx (KindScheme _ ty) = do
      fst <$> go IMap.empty ty
    where
      go :: IntMap (Kind 'Eval) -> Kind 'Pure -> IO (Kind 'Eval, IntMap (Kind 'Eval))
      go kindVarMap = \case
        Star -> do
          pure (Star, kindVarMap)
        KFun ki ki' -> do
          (res, map')   <- go kindVarMap ki
          (res', map'') <- go map' ki'
          pure (KFun res res', map'')
        KGen s -> do
          case IMap.lookup s kindVarMap of
            Just hole -> pure (hole, kindVarMap)
            Nothing   -> do
              kind <- newHole ctx
              pure (kind, IMap.insert s kind kindVarMap)

-- | Transforms an impuer kind eval with holes into a kind scheme
generalize :: Kind 'Eval -> IO KindScheme
generalize kindEval = do
    (pureKind, count, _) <- go [] 0 kindEval
    pure (KindScheme count pureKind)
  where
    go :: [(HoleRef, Int)] -> Int -> Kind 'Eval -> IO (Kind 'Pure, Int, [(HoleRef, Int)])
    go map' count = \case
      Star -> pure (Star, count, map')
      KFun ki ki' -> do
        (kindRes, nCount, nMap)    <- go map' count ki
        (kindRes', nCount', nMap') <- go nMap nCount ki'
        pure (KFun kindRes kindRes', nCount', nMap')
      KHole hole -> do
        holeRes <- readIORef hole
        case holeRes of
          Empty _ ->
            case lookup hole map' of
              Nothing -> pure (KGen count, count + 1, (hole,count) : map')
              Just n -> pure (KGen n, count, map')
          Filled ki -> go map' count ki
      KLoc _ ki -> go map' count ki

findType :: Ctx -> Bounds -> Text -> IO (Type 'Eval)
findType Ctx { ctxVars = vars } pos key = case Map.lookup key vars of
  Nothing -> throwIO (CantFindVar pos key)
  Just ki -> pure ki

findKind :: Ctx -> Bounds -> Text -> IO (Kind 'Eval)
findKind ctx@Ctx { ctxTypes = types } pos key = case Map.lookup key types of
  Nothing -> throwIO (CantFindKind pos key)
  Just ki -> instantiate ctx ki

findKindEval :: Ctx -> Bounds -> Text -> IO (Kind 'Eval)
findKindEval Ctx { ctxHoles = holes } pos key = case Map.lookup key holes of
  Nothing -> throwIO (CantFindKind pos key)
  Just ki -> pure ki

unifyHoles :: HoleRef -> HoleRef -> IO ()
unifyHoles h h' = do
  valueH  <- readIORef h
  valueH' <- readIORef h'
  case (valueH, valueH') of
    (Filled a, _) -> unifyHole True h' a
    (_, Filled a) -> unifyHole False h a
    (Empty lvlH, Empty lvlH') ->
      if lvlH > lvlH'
        then writeIORef h (Filled (KHole h'))
        else writeIORef h' (Filled (KHole h))

unifyHole :: Bool -> HoleRef -> Kind 'Eval -> IO ()
unifyHole inverted hole kind = do
    holeInt <- readIORef hole
    case holeInt of
      Filled t -> unify t kind
      Empty  lvl -> do
        catch (occoursCheck lvl kind) $ \case
          OccoursCheckKind _ _ -> throwIO $
            if inverted
              then OccoursCheckKind kind (KHole hole)
              else OccoursCheckKind (KHole hole) kind
          other            -> throwIO other
        writeIORef hole (Filled kind)
  where
    occoursCheck :: Lvl -> Kind 'Eval -> IO ()
    occoursCheck lvl = \case
      KLoc _ s -> occoursCheck lvl s
      Star -> pure ()
      KFun ki ki' -> occoursCheck lvl ki >> occoursCheck lvl ki'
      KHole hole' -> do
        if hole == hole'
          then throwIO $ if inverted
                  then OccoursCheckKind (KHole hole) (KHole hole')
                  else OccoursCheckKind (KHole hole') (KHole hole)
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
  case (t, t') of
    (Star, Star)                  -> pure ()
    (KHole h, KHole h') | h == h' -> pure ()
    (KFun a b, KFun a' b') -> unify a a' >> unify b b'
    (KHole h, KHole h')    -> unifyHoles h h'
    (b', KHole h)          -> unifyHole True h b'
    (KHole h, b')          -> unifyHole False h b'
    (KLoc _ a, KLoc _ b)   -> unify a b
    (KLoc _ a, b)          -> unify a b
    (a, KLoc _ b)          -> unify a b
    _                      -> throwIO (CantUnifyKind t t')

check :: Ctx -> Expr.Typer Normal -> Kind 'Eval -> IO (Type 'Eval)
check ctx ty kind = do
    (ty', kind') <- infer ctx ty
    catch (unify kind kind') $ \case
      CantUnifyKind fn@KFun {} _ -> throwIO $ NotATypeConstructor kind'
      CantUnifyKind _ fn@KFun {} -> throwIO $ NotEnoughArgs kind'
      CantUnifyKind _ _          -> throwIO $ CantUnifyKind kind kind'
      OccoursCheckKind _ _ -> throwIO $ OccoursCheckKind kind kind'
      other -> throwIO other
    pure ty'

infer :: Ctx -> Expr.Typer Normal -> IO (Type 'Eval, Kind 'Eval)
infer ctx t = do
  case t of
    Expr.TSimple _ (Expr.Name pos id') -> do
      kind <- findKind ctx pos id'
      pure (TyLoc pos $ TyNamed id', KLoc pos kind)
    Expr.TPoly _ (Expr.Name pos id') -> do -- To bounded vars
      kind <- findKindEval ctx pos id'
      pure (TyNamed id', KLoc pos kind)
    Expr.TArrow pos ty ty' -> do
      tyFrom <- check ctx ty Star
      tyTo <- check ctx ty' Star
      pure (TyLoc pos $ TyFun tyFrom tyTo, KLoc pos Star)
    Expr.TApp pos fTy argTy'   -> do
      holeArg <- newHole ctx
      holeRet <- newHole ctx
      tyArg <- check ctx fTy (KFun holeArg holeRet)
      tyRet <- check ctx argTy' holeArg
      pure (TyLoc pos $ TyApp holeRet tyArg tyRet, KLoc pos holeRet)
    Expr.TForall pos na ty -> do
      binderHole <- newHole ctx
      let newCtx = upKindLvl $ addToEval ctx (Expr.getId na) (KLoc pos binderHole)
      (t', k)    <- infer newCtx ty
      pure (TyLoc pos $ TyForall (Expr.getId na) t', KLoc pos $ KFun binderHole k)


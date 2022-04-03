module Typer.Kinds (
  newHole,
  HoleRef,
  instantiate,
  generalize,
  findType,
  findKind,
  findKindEval,
  unifyHoles,
  unifyHole,
  unify,
  check,
  infer,
) where

import Data.Text         (Text)
import Data.IntMap       (IntMap)
import Data.IORef        (IORef, readIORef, newIORef, writeIORef)
import Control.Exception (throwIO)
import Syntax.Parser.Ast (Normal)
import Syntax.Range      (Range)
import Typer.Types       (KindScheme(..), Hole(..), Kind(..), Lvl, EvalStatus(..), TType(..), Loc (Ghost, Loc))
import Typer.Context     (addToEval, upKindLvl, Ctx(..))
import Typer.Errors      (TypeError(..))
import Typer.Tracer
import Control.Monad.IO.Class (liftIO)

import qualified Data.Map    as Map
import qualified Data.IntMap as IMap
import qualified Syntax.Expr as Expr
import qualified Syntax.Range as Ranged
import qualified Syntax.Parser.Ast as Ast

type HoleRef = IORef (Hole (Kind 'Eval))

newHole :: MonadTyper m => Loc -> Ctx -> m (Kind 'Eval)
newHole loc ctx = do
  ref <- liftIO $ newIORef (Empty (ctxKindLvl ctx))
  pure (KHole loc ref)

-- | Tries to transform a kind scheme to a normal eval kind 
instantiate :: MonadTyper m => Ctx -> KindScheme -> m (Kind 'Eval)
instantiate ctx (KindScheme _ ty) = do
      fst <$> go IMap.empty ty
    where
      go :: MonadTyper m => IntMap (Kind 'Eval) -> Kind 'Pure -> m (Kind 'Eval, IntMap (Kind 'Eval))
      go kindVarMap = \case
        Star loc -> do
          pure (Star loc, kindVarMap)
        KFun loc ki ki' -> do
          (res, map')   <- go kindVarMap ki
          (res', map'') <- go map' ki'
          pure (KFun loc res res', map'')
        KGen loc s -> do
          case IMap.lookup s kindVarMap of
            Just hole -> pure (hole, kindVarMap)
            Nothing   -> do
              kind <- newHole loc ctx
              pure (kind, IMap.insert s kind kindVarMap)

-- | Transforms an impuer kind eval with holes into a kind scheme
generalize :: Kind 'Eval -> IO KindScheme
generalize kindEval = do
    (pureKind, count, _) <- go [] 0 kindEval
    pure (KindScheme count pureKind)
  where
    go :: [(HoleRef, Int)] -> Int -> Kind 'Eval -> IO (Kind 'Pure, Int, [(HoleRef, Int)])
    go map' count = \case
      Star loc -> pure (Star loc, count, map')
      KFun loc ki ki' -> do
        (kindRes, nCount, nMap)    <- go map' count ki
        (kindRes', nCount', nMap') <- go nMap nCount ki'
        pure (KFun loc kindRes kindRes', nCount', nMap')
      KHole loc hole -> do
        holeRes <- readIORef hole
        case holeRes of
          Empty _ ->
            case lookup hole map' of
              Nothing -> pure (KGen loc count, count + 1, (hole,count) : map')
              Just n -> pure (KGen loc n, count, map')
          Filled ki -> go map' count ki

findType :: MonadTyper m => Ctx -> Range -> Text -> m (TType 'Eval)
findType Ctx { ctxVars = vars } pos key = case Map.lookup key vars of
  Nothing -> liftIO $ throwIO (CantFindVar pos key)
  Just ki -> pure ki

findKind :: MonadTyper m => Ctx -> Range -> Text -> m (Kind 'Eval)
findKind ctx@Ctx { ctxTypes = types } pos key = case Map.lookup key types of
  Nothing -> liftIO $ throwIO (CantFindKind pos key)
  Just ki -> instantiate ctx ki

findKindEval :: MonadTyper m => Ctx -> Range -> Text -> m (Kind 'Eval)
findKindEval Ctx { ctxHoles = holes } pos key = case Map.lookup key holes of
  Nothing -> liftIO $ throwIO (CantFindKind pos key)
  Just ki -> pure ki

unifyHoles :: MonadTyper m => Loc -> Loc -> HoleRef -> HoleRef -> m ()
unifyHoles l l' h h' = do
  valueH  <- liftIO $ readIORef h
  valueH' <- liftIO $ readIORef h'
  case (valueH, valueH') of
    (Filled a, _) -> unifyHole l' True h' a
    (_, Filled a) -> unifyHole l False h a
    (Empty lvlH, Empty lvlH') ->
      if lvlH > lvlH'
        then liftIO $ writeIORef h (Filled (KHole l' h'))
        else liftIO $ writeIORef h' (Filled (KHole l h))

unifyHole :: MonadTyper m => Loc -> Bool -> HoleRef -> Kind 'Eval -> m ()
unifyHole loc inverted hole kind = do
    holeInt <- liftIO $ readIORef hole
    case holeInt of
      Filled t -> unify t kind
      Empty  lvl -> do
        occoursCheck lvl kind
        liftIO $ writeIORef hole (Filled kind)
  where
    occoursCheck :: MonadTyper m => Lvl -> Kind 'Eval -> m ()
    occoursCheck lvl = \case
      Star _ -> pure ()
      KFun _ ki ki' -> occoursCheck lvl ki >> occoursCheck lvl ki'
      KHole p hole' -> do
        if hole == hole'
          then liftIO $ throwIO $ if inverted
                  then OccoursCheckKind (KHole loc hole) (KHole p hole')
                  else OccoursCheckKind (KHole p hole') (KHole loc hole)
          else pure ()
        resHole' <- liftIO $ readIORef hole'
        case resHole' of
          Empty lvlHole' ->
            if lvlHole' > lvl
              then liftIO $ writeIORef hole' (Empty lvl)
              else pure ()
          Filled ki -> occoursCheck lvl ki

unify :: MonadTyper m => Kind 'Eval -> Kind 'Eval -> m ()
unify t t' = scope (UnifyKind t t') $ do
  case (t, t') of
    (Star _, Star _)                  -> pure ()
    (KHole _ h, KHole _ h') | h == h' -> pure ()
    (KFun _ a b, KFun _ a' b') -> unify a a' >> unify b b'
    (KHole l h, KHole l' h')    -> unifyHoles l l' h h'
    (b', KHole l h)          -> unifyHole l True h b'
    (KHole l h, b')          -> unifyHole l False h b'
    _                      -> do
      fstSub <- lastUnifyStep
      most <- getInnermostType
      liftIO $ throwIO
             $ maybe (CantUnifyKind most t t')
                     (uncurry $ CantUnifyKind most)
                     fstSub

check :: MonadTyper m => Ctx -> Expr.Typer Normal -> Kind 'Eval -> m (TType 'Eval)
check ctx ty kind = scope (CheckKind ty kind) $ do
    (ty', kind') <- infer ctx ty
    unify kind kind'
    pure ty'

infer :: MonadTyper m => Ctx -> Expr.Typer Normal -> m (TType 'Eval, Kind 'Eval)
infer ctx t = scope (InferKind t) $ do
  case t of
    Expr.TSimple _ (Expr.Name pos id') -> do
      kind <- findKind ctx pos id'
      pure (TyNamed id', kind)
    Expr.TPoly _ (Expr.Name pos id') -> do -- To bounded vars
      kind <- findKindEval ctx pos id'
      pure (TyNamed id', kind)
    Expr.TArrow pos ty ty' -> do
      tyFrom <- check ctx ty (Star $ Loc $ Aat.getPos ty)
      tyTo <- check ctx ty' (Star $ Loc $ Aat.getPos ty')
      pure (TyFun tyFrom tyTo, Star $ Loc pos)
    Expr.TApp pos fTy argTy'   -> do
      holeArg <- newHole (Loc $ Ast.getPos fTy) ctx
      holeRet <- newHole (Loc $ Ast.getPos argTy') ctx
      tyArg <- check ctx fTy (KFun (Loc pos) holeArg holeRet)
      tyRet <- check ctx argTy' holeArg
      pure (TyApp holeRet tyArg tyRet, holeRet)
    Expr.TForall pos na ty -> do
      binderHole <- newHole (Loc $ Ast.getPos na) ctx
      let newCtx = upKindLvl $ addToEval ctx pos (Expr.getId na) binderHole
      (t', k)    <- infer newCtx ty
      pure (TyForall (Expr.getId na) t', k)


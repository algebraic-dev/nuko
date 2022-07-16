module Nuko.Typer.Infer.Type (
  inferTy,
  inferRealTy
) where

import Relude               ((.), id)
import Relude.Base          (Eq(..))
import Relude.Unsafe        ((!!))
import Relude.Applicative   (pure)
import Relude.Monad         (Maybe(..))

import Control.Monad.Reader (Monad, foldM, asks, MonadReader(local), MonadTrans(lift), ReaderT)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.List            (findIndex)

import Nuko.Typer.Error     (TypeError(..))
import Nuko.Typer.Unify     (unifyKind)
import Nuko.Typer.Types     (TKind(..), TTy(..), Relation(..), generalizeWith)
import Nuko.Typer.Env       (getKind, newKindHole, MonadTyper, qualifyPath)
import Nuko.Tree.Expr       (Ty(..))
import Nuko.Utils           (terminate)
import Nuko.Names           (genIdent, mkTyName, Label(Label), Name, TyName )
import Nuko.Tree            (Re)

import qualified Control.Monad.State  as StateT
import qualified Control.Monad.Reader as ReaderT

type PType x = (TTy x, TKind)

type InferTy m a = StateT.StateT [Name TyName] (ReaderT [(Name TyName, TKind)] m) a

runInferTy :: InferTy m a -> [(Name TyName, TKind)] -> m (a, [Name TyName])
runInferTy act poly = ReaderT.runReaderT (StateT.runStateT act []) poly

heft :: Monad m => m a -> InferTy m a
heft = lift . lift

inferTy :: MonadTyper m => [(Name TyName, TKind)] -> Ty Re -> m (PType 'Virtual)
inferTy poly ast = do
  ((pTy, pKind), freeNames) <- inferRealTy poly ast
  pure (generalizeWith freeNames pTy id, pKind)

-- TODO: Probably i can just remove poly and use a readerT instead?
inferRealTy :: MonadTyper m => [(Name TyName, TKind)] -> Ty Re -> m (PType 'Real, [Name TyName])
inferRealTy poly ast = runInferTy (go ast) poly
  where
    findName :: MonadTyper m => Name TyName -> InferTy m (PType 'Real)
    findName name = do
      result <- asks (findIndex (\(k, _) -> k == name))
      case result of
        Nothing -> terminate (NameResolution (Label name))
        Just idx -> do
          (_, kPair) <- asks (!! idx)
          pure (TyVar idx, kPair)

    applyTy :: MonadTyper m => PType 'Real -> Ty Re -> InferTy m (PType 'Real)
    applyTy (tyRes, tyKind) arg = do
      (argTyRes, argTyKind) <- go arg
      resHole <- heft (newKindHole (mkTyName (genIdent "")))
      heft (unifyKind tyKind (KiFun argTyKind resHole))
      pure (TyApp resHole tyRes argTyRes, resHole)

    go :: MonadTyper m => Ty Re -> InferTy m (PType 'Real)
    go = \case
      TId path _ -> do
        kind <- heft (getKind path)
        qualified <- heft (qualifyPath path)
        pure (TyIdent qualified, kind)
      TPoly name _ ->
        findName name
      TApp ty (x :| xs) _ -> do
        res <- go ty
        foldM applyTy res (x : xs)
      TArrow from to _ -> do
        (vFrom, vFromKi) <- go from
        (vTo, vToKi) <- go to
        heft (unifyKind vFromKi KiStar)
        heft (unifyKind vToKi KiStar)
        pure (TyFun vFrom vTo, KiStar)
      TForall name ty _ -> do
        hole <- heft (newKindHole name)
        (resTy, resKind) <- local ((name, hole) :) (go ty)
        pure (TyForall name resTy, resKind)
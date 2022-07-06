module Nuko.Typer.Infer.Type (
  inferTy
) where

import Data.List          (findIndex, (!!))
import Relude.Lifted (newIORef)
import Control.Monad (foldM)

import Relude             (MonadTrans (lift), NonEmpty ((:|)), Eq ((==)))
import Relude.Monad       (asks, ReaderT, MonadReader (local), Maybe (..))
import Relude.String      (Text)
import Relude.Functor     ((<$>))
import Relude.Applicative (Applicative(pure, (<*>)))

import Nuko.Utils         (terminate)
import Nuko.Tree          (Ty(..), Re)
import Nuko.Typer.Env     (MonadTyper, getKind)
import Nuko.Typer.Error   (TypeError(NameResolution))
import Nuko.Typer.Types   (PType, TTy (..), TKind (..), Hole (..))
import Nuko.Typer.Unify   (unifyKind)
import Nuko.Resolver.Tree (ReId(text))

import qualified Control.Monad.Reader as Reader

inferTy :: MonadTyper m => Ty Re -> m PType
inferTy ast =
    Reader.runReaderT (go ast) []
  where
    go :: MonadTyper m => Ty Re -> ReaderT ([(Text, TKind)]) m PType
    go = \case
      TId path _ -> do
        kind   <- lift (getKind path)
        let vty = TyIdent path
        pure (vty, kind)
      TPoly name _ -> do
        result <- asks (findIndex (\(k, _) -> k == name.text))
        case result of
          Just idx -> do
            (_, kind) <- asks (!! idx) -- It's safe because we already found it
            pure (TyVar idx, kind)
          Nothing   -> terminate (NameResolution name.text)
      TApp ty (x :| xs) _  -> do
        res <- go ty
        foldM (\(tyRes, tyKind) arg -> do
            (argTyRes, argKind) <- go arg
            resHole <- KiHole <$> newIORef (Empty "" 0)
            lift (unifyKind tyKind (KiFun argKind resHole))
            pure (TyApp tyRes argTyRes, resHole)
          ) res (x : xs)
      TArrow from to _     -> do
        ((vFrom, vFromKi), (vTo, vToKi)) <- (,) <$> go from <*> go to
        lift (unifyKind vFromKi KiStar)
        lift (unifyKind vToKi KiStar)
        pure (TyFun vFrom vTo, KiStar)
      TForall name ty _    -> do
        hole <- KiHole <$> newIORef (Empty name.text 0)
        local ((name.text,hole) :) (go ty)
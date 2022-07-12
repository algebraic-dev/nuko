module Nuko.Typer.Infer.Type (
  inferTy,
  findCycle,
  freeVars,
) where

import Data.List          (findIndex, (!!))
import Control.Monad      (foldM, when)

import Relude             (NonEmpty ((:|)), Eq ((==)), fst, show, ($), StateT, Applicative ((*>)), Traversable (traverse))
import Relude.Monad       (asks, ReaderT, MonadReader (local, ask), Maybe (..), MonadTrans (lift))
import Relude.String      (Text)
import Relude.Monoid      (Semigroup ((<>)))
import Relude.Functor     ((<$>))
import Relude.Lifted      (newIORef)
import Relude.Foldable    (traverse_, Foldable (foldr))
import Relude.Container   (HashMap, HashSet)
import Relude.Applicative (Applicative(pure, (<*>)))

import Nuko.Utils         (terminate)
import Nuko.Tree          (Ty(..), Re)
import Nuko.Typer.Env     (MonadTyper, getKind)
import Nuko.Typer.Error   (TypeError(..))
import Nuko.Typer.Types   (PType, TTy (..), TKind (..), Hole (..), implTy)
import Nuko.Typer.Unify   (unifyKind)
import Nuko.Resolver.Tree (ReId(text), Path (..))

import qualified Control.Monad.Reader       as Reader
import qualified Control.Monad.State.Strict as State
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.HashSet as HashSet

freeVars :: MonadTyper m => Ty Re -> m (HashSet Text)
freeVars = \case
  TPoly name _  -> pure (HashSet.singleton name.text)
  TId _ _       -> pure (HashSet.empty)
  TApp a args _ -> do
    argsSet <- foldr (<>) HashSet.empty <$> (traverse freeVars args)
    exprSet <- freeVars a
    pure (argsSet <> exprSet)
  TArrow from to _ -> do
    (<>) <$> freeVars from <*> freeVars to
  TForall name ty _ -> do
    tySet <- freeVars ty
    pure (HashSet.delete name.text tySet)

inferTy :: MonadTyper m => [(Text, TKind)] -> Ty Re -> m PType
inferTy poly ast =
    Reader.runReaderT (go ast) poly
  where
    go :: MonadTyper m => Ty Re -> ReaderT ([(Text, TKind)]) m PType
    go = \case
      TId path _ -> do
        kind   <- lift (getKind path)
        let vty = TyIdent path
        pure (vty, kind)
      TPoly name _ -> do
        result <- asks (findIndex (\(k, _) -> k == name.text))
        rere <- ask
        case result of
          Just idx -> do
            (_, kind) <- asks (!! idx) -- It's safe because we already found it
            pure (TyVar idx, kind)
          Nothing   -> terminate (NameResolution $ name.text <> "|" <> (show (fst <$> rere)))
      TApp ty (x :| xs) _  -> do
        res <- go ty
        foldM (\(tyRes, tyKind) arg -> do
            (argTyRes, argKind) <- go arg
            resHole <- KiHole <$> newIORef (Empty "" 0)
            lift (unifyKind tyKind (KiFun argKind resHole))
            pure (TyApp resHole tyRes argTyRes, resHole)
          ) res (x : xs)
      TArrow from to _ -> do
        ((vFrom, vFromKi), (vTo, vToKi)) <- (,) <$> go from <*> go to
        lift (unifyKind vFromKi KiStar)
        lift (unifyKind vToKi KiStar)
        pure (TyFun vFrom vTo, KiStar)
      TForall name ty _ -> do
        hole <- KiHole <$> newIORef (Empty name.text 0)
        (resTy, resKind) <- local ((name.text,hole) :) (go ty)
        pure (TyForall name.text (\f -> implTy f resTy), resKind)

-- This is some kind of DFS but for a directed graph that the vertex
-- are just other types in the same recursive group as the the type syn
-- we are testing.

data Status = Visiting | Visited

findCycle :: MonadTyper m => Text -> Text -> HashMap Text (Ty Re) -> Ty Re -> m ()
findCycle curMod name recursiveGroup ty = do
    State.evalStateT (go [name] ty) (HashMap.singleton name Visiting)
  where
    setStatus :: MonadTyper m => ReId -> Status -> StateT (HashMap Text Status) m ()
    setStatus name' status = State.modify (HashMap.insert name'.text status)

    go :: MonadTyper m => [Text] -> Ty Re -> StateT (HashMap Text Status) m ()
    go stack = \case
      TId (Path mod name' _) _  -> do
        when (mod == curMod) $ do
          env <- State.get
          case HashMap.lookup name'.text env of
            Nothing       -> do
              setStatus name' Visiting
              case HashMap.lookup name'.text recursiveGroup of
                Just tyRes -> go (name'.text : stack) tyRes *> setStatus name' Visited
                Nothing    -> pure ()
            Just Visiting -> lift (terminate $ CyclicTypeDef stack)
            Just Visited  -> pure ()
      TApp ty' args _   -> go stack ty' *> traverse_ (go stack) args
      TArrow from to _  -> go stack from *> go stack to
      TForall _ ty' _   -> go stack ty'
      _                 -> pure ()

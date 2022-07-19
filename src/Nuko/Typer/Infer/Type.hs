module Nuko.Typer.Infer.Type (
  inferClosedTy,
  inferOpenTy,
  freeVars
) where

import Relude               (id, snd, (<$>), HashSet, Traversable (traverse), ($))
import Relude.Base          (Eq(..))
import Relude.Unsafe        ((!!))
import Relude.Applicative   (pure)
import Relude.Monad         (Maybe(..))

import Control.Monad.Reader (foldM)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.List            (findIndex, zip)

import Nuko.Typer.Error     (TypeError(..))
import Nuko.Typer.Unify     (unifyKind)
import Nuko.Typer.Types     (TKind(..), TTy(..), Relation(..), generalizeWith)
import Nuko.Typer.Env       (getKind, newKindHole, MonadTyper, qualifyPath, addLocalTy, seTyEnv, addLocalTypes, terminateLocalized)
import Nuko.Tree.Expr       (Ty(..))
import Nuko.Names           (genIdent, mkTyName, Label(Label), Name, TyName )
import Nuko.Tree            (Re)

import Lens.Micro.Platform (view)
import qualified Data.HashSet as HashSet
import Nuko.Report.Range (getPos)

type PType x = (TTy x, TKind)

inferOpenTy :: MonadTyper m => Ty Re -> m (PType 'Real)
inferOpenTy = inferRealTy

inferClosedTy :: MonadTyper m => Ty Re -> m (PType 'Virtual)
inferClosedTy ast = do
  let fv = HashSet.toList (freeVars ast)
  holes <- traverse newKindHole fv
  addLocalTypes (zip fv holes) $ do
    (pTy, pKind) <- inferRealTy ast
    pure (generalizeWith fv pTy id, pKind)

freeVars :: Ty Re -> HashSet (Name TyName)
freeVars = \case
  TId _ _ -> HashSet.empty
  TPoly name _ -> HashSet.singleton name
  TApp ty (x :| xs) _ -> HashSet.unions (freeVars <$> (ty : x : xs))
  TArrow from to _ -> HashSet.union (freeVars from) (freeVars to)
  TForall name ty _ -> HashSet.delete name (freeVars ty)

-- TODO: Probably i can just remove poly and use a readerT instead?
-- TODO: Disallow Guarded polimorphic types
inferRealTy :: MonadTyper m => Ty Re -> m (PType 'Real)
inferRealTy ast = go ast
  where
    findName :: MonadTyper m => Name TyName -> m (PType 'Real)
    findName name = do
      env <- view seTyEnv
      case findIndex (\(k, _) -> k == name) env of
        Just idx -> do
          pure (TyVar idx, snd (env !! idx))
        Nothing -> terminateLocalized (NameResolution (Label name)) (Just $ getPos name)

    applyTy :: MonadTyper m => PType 'Real -> Ty Re -> m (PType 'Real)
    applyTy (tyRes, tyKind) arg = do
      (argTyRes, argTyKind) <- go arg
      resHole <- newKindHole (mkTyName (genIdent ""))
      unifyKind tyKind (KiFun argTyKind resHole)
      pure (TyApp resHole tyRes argTyRes, resHole)

    go :: MonadTyper m => Ty Re -> m (PType 'Real)
    go = \case
      TId path _ -> do
        kind <- getKind path
        qualified <- qualifyPath path
        pure (TyIdent qualified, kind)
      TPoly name _ ->
        findName name
      TApp ty (x :| xs) _ -> do
        res <- go ty
        foldM applyTy res (x : xs)
      TArrow from to _ -> do
        (vFrom, vFromKi) <- go from
        (vTo, vToKi) <- go to
        unifyKind vFromKi KiStar
        unifyKind vToKi KiStar
        pure (TyFun vFrom vTo, KiStar)
      TForall name ty _ -> do
        hole <- newKindHole name
        (resTy, resKind) <- addLocalTy name hole (go ty)
        pure (TyForall name resTy, resKind)
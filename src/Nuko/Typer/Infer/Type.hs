module Nuko.Typer.Infer.Type (
  inferClosedTy,
  inferOpenTy,
  freeVars
) where

import Relude

import Nuko.Names           (Label (Label), Name, TyName, genIdent, mkTyName)
import Nuko.Report.Range    (Range (..), getPos)
import Nuko.Tree            (Re)
import Nuko.Tree.Expr       (Ty (..))
import Nuko.Typer.Env       (MonadTyper, addLocalTy, addLocalTypes,
                             endDiagnostic, getKind, newKindHole, qualifyPath,
                             seTyEnv)
import Nuko.Typer.Error     (TypeError (..))
import Nuko.Typer.Types     (Relation (..), TKind (..), TTy (..),
                             generalizeWith)
import Nuko.Typer.Unify     (unifyKind)

import Control.Monad.Reader (foldM)
import Data.HashSet         qualified as HashSet
import Data.List            (findIndex, (!!))
import Lens.Micro.Platform  (view)


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
  TId _ _             -> HashSet.empty
  TPoly name _        -> HashSet.singleton name
  TApp ty (x :| xs) _ -> HashSet.unions (freeVars <$> (ty : x : xs))
  TArrow from to _    -> HashSet.union (freeVars from) (freeVars to)
  TForall name ty _   -> HashSet.delete name (freeVars ty)

-- TODO: Probably i can just remove poly and use a readerT instead?
-- TODO: Disallow Guarded polimorphic types
inferRealTy :: MonadTyper m => Ty Re -> m (PType 'Real)
inferRealTy = go
  where
    findName :: MonadTyper m => Name TyName -> m (PType 'Real)
    findName name = do
      env <- view seTyEnv
      case findIndex (\(k, _) -> k == name) env of
        Just idx -> do
          pure (TyVar idx, snd (env !! idx))
        Nothing -> endDiagnostic (NameResolution (getPos name) (Label name)) (getPos name)

    applyTy :: MonadTyper m => Range -> PType 'Real -> Ty Re -> m (PType 'Real)
    applyTy range (tyRes, tyKind) arg = do
      (argTyRes, argTyKind) <- go arg
      resHole <- newKindHole (mkTyName (genIdent ""))
      unifyKind range tyKind (KiFun argTyKind resHole)
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
        foldM (applyTy (getPos ty)) res (x : xs)
      TArrow from to _ -> do
        (vFrom, vFromKi) <- go from
        (vTo, vToKi) <- go to
        unifyKind (getPos from) vFromKi KiStar
        unifyKind (getPos to)   vToKi KiStar
        pure (TyFun vFrom vTo, KiStar)
      TForall name ty _ -> do
        hole <- newKindHole name
        (resTy, resKind) <- addLocalTy name hole (go ty)
        pure (TyForall name resTy, resKind)

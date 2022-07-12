module Nuko.Typer.Infer.LetDecl (
  initLetDecl,
  inferLetDecl
) where

import Relude                (snd, fst, Semigroup ((<>)), zip, (.), ($), Text)
import Relude.Functor        (Functor(fmap), (<$>))
import Relude.Foldable       (Foldable(foldr), Traversable(traverse), traverse_)
import Relude.Applicative    (Applicative(pure))

import Nuko.Typer.Tree       () -- Just to make the equality between XName Re and XName Tc works
import Nuko.Tree             (LetDecl(..), Re, Tc)
import Nuko.Typer.Env        (MonadTyper, addTy, newKindHole, tsVars, addLocals, newTyHole)
import Nuko.Typer.Types      (TKind (..), TTy (..), generalizeOver, Virtual, PType, subs)
import Nuko.Resolver.Tree    (ReId(text))
import Nuko.Typer.Infer.Type (inferTy, freeVars)
import Nuko.Typer.Unify      (unifyKind)

import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Nuko.Typer.Infer.Expr (checkExpr)

data InitLetDecl = InitLetDecl
  { bindings  :: [PType]
  , retType   :: TTy Virtual
  , iFreeVars :: [Text]
  }

initLetDecl :: MonadTyper m => LetDecl Re -> m InitLetDecl
initLetDecl decl = do
  let argRawTypes = fmap snd decl.declArgs
  freeVarsSet    <- HashSet.toList <$> foldr (<>) HashSet.empty <$> traverse freeVars argRawTypes
  newVars        <- traverse newKindHole freeVarsSet
  let bindings    = zip freeVarsSet newVars
  argsBindings   <- traverse (inferTy bindings) argRawTypes
  traverse_ (`unifyKind` KiStar) (snd <$> argsBindings)

  (retType, retKind) <- inferTy bindings decl.declRet
  unifyKind retKind KiStar

  let fnType = foldr TyFun retType (fst <$> argsBindings)

  let generalizedTy = generalizeOver freeVarsSet fnType
  addTy tsVars decl.declName.text generalizedTy
  pure (InitLetDecl argsBindings retType freeVarsSet)

inferLetDecl :: MonadTyper m => LetDecl Re -> InitLetDecl -> m (LetDecl Tc)
inferLetDecl (LetDecl name args body _ e) init = do
  holes <- traverse newTyHole init.iFreeVars
  let argTys = (subs 0 holes . fst <$> init.bindings)
  let bindings = HashMap.fromList $ zip ((text . fst) <$> args) argTys
  resBody <- addLocals bindings (checkExpr body init.retType)
  pure (LetDecl name ((\((name', _), (ty, _)) -> (name', ty)) <$> zip args init.bindings) resBody init.retType e)
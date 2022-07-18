module Nuko.Typer.Infer.LetDecl (
  initLetDecl,
  inferLetDecl
) where

import Relude                   ((<$>), Traversable (traverse), Foldable (foldr), snd, traverse_, ($), fst, Maybe (Nothing))
import Relude.Applicative       (pure)
import Nuko.Resolver.Tree       ()
import Nuko.Typer.Tree          ()

import Nuko.Typer.Env           (tsVars, MonadTyper, addTy, DefInfo(..), newKindHole, addLocalTypes, addLocals, newTyHole)

import Nuko.Typer.Infer.Type    (inferOpenTy, freeVars)
import Nuko.Typer.Unify         (unifyKind)
import Nuko.Typer.Types         (TTy(..), TKind(KiStar), evaluate, generalizeNames )

import Nuko.Tree                (LetDecl(..), Tc, Re)

import Data.List (unzip, zip)

import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Nuko.Typer.Infer.Expr (checkExpr)
import Nuko.Names (NameKind(..), coerceTo)

initLetDecl :: MonadTyper m => LetDecl Re -> m DefInfo
initLetDecl (LetDecl name args _ ret _) = do

  -- Joins all the free variables inside the type just to discover the polimorphic bindinds
  let freeTys = HashSet.toList (HashSet.unions (freeVars <$> (ret : (snd <$> args))))
  holes <- traverse newKindHole freeTys
  let bindings = zip freeTys holes

  addLocalTypes bindings $ do
    (argsReal, argsKind)  <- unzip <$> traverse inferOpenTy (snd <$> args)
    (retRet, retKind) <- inferOpenTy ret

    traverse_ (`unifyKind` KiStar) (retKind : argsKind)
    let realTy = foldr TyFun retRet argsReal
    let generalizedTy = generalizeNames freeTys realTy

    let info = DefInfo generalizedTy bindings argsReal retRet
    addTy tsVars Nothing name (generalizedTy, info)
    pure info

inferLetDecl :: MonadTyper m => LetDecl Re -> DefInfo -> m (LetDecl Tc)
inferLetDecl (LetDecl name' args arg _ ext) tyInfo = do
  addLocalTypes tyInfo._polymorphics $ do
    let names = fst <$> args
    let argNamesReal = zip names tyInfo._argTypes
    polyTys <- traverse newTyHole (coerceTo TyName <$> names)
    let argNames = HashMap.fromList $ zip (fst <$> args) (evaluate polyTys <$> tyInfo._argTypes)
    addLocals argNames $ do
      bodyRes <- checkExpr arg (evaluate [] tyInfo._retType)
      pure (LetDecl name' argNamesReal bodyRes tyInfo._retType ext)
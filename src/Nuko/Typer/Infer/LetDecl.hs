module Nuko.Typer.Infer.LetDecl (
  initLetDecl,
  inferLetDecl
) where

import Relude

import Nuko.Names            (NameKind (..), coerceTo)
import Nuko.Report.Range     (getPos)
import Nuko.Tree             (LetDecl (..), Re, Tc)
import Nuko.Typer.Env        (DefInfo (..), MonadTyper, addLocalTypes,
                              addLocals, addTy, newKindHole, newTyHole, tsVars)
import Nuko.Typer.Infer.Expr (checkExpr)
import Nuko.Typer.Infer.Type (freeVars, inferOpenTy)
import Nuko.Typer.Types      (TKind (KiStar), TTy (..), evaluate,
                              generalizeNames)
import Nuko.Typer.Unify      (unifyKind)

import Data.HashMap.Strict   qualified as HashMap
import Data.HashSet          qualified as HashSet

initLetDecl :: MonadTyper m => LetDecl Re -> m DefInfo
initLetDecl (LetDecl name args _ ret _) = do

  -- Joins all the free variables inside the type just to discover the polimorphic bindinds
  let freeTys = HashSet.toList (HashSet.unions (freeVars <$> (ret : (snd <$> args))))
  holes <- traverse newKindHole freeTys
  let bindings = zip freeTys holes

  addLocalTypes bindings $ do
    (argsReal, argsKind)  <- unzip <$> traverse (\res -> (\(t, k) -> (t, (getPos res, k))) <$> inferOpenTy res) (snd <$> args)
    (retRet, retKind) <- inferOpenTy ret

    traverse_ (\(range, kind) -> unifyKind range kind KiStar) ((getPos ret, retKind) : argsKind)
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

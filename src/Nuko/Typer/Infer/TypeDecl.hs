module Nuko.Typer.Infer.TypeDecl (
  initTypeDecl,
  inferTypeDecl
) where

import Relude                   ((<$>), Traversable (traverse), Foldable (foldr, foldl'), snd, fst, error, traverse_, id, Num ((-)))
import Relude.Functor           (Functor (fmap))
import Relude.Applicative       (pure)


import Nuko.Resolver.Tree       ()
import Nuko.Typer.Tree          ()

import Nuko.Typer.Env
  ( tsConstructors
  , addTy, addFieldToEnv, addTyKind , qualifyPath, newKindHole
  , DataConsInfo(DataConsInfo), FieldInfo(FieldInfo), TyInfoKind(IsTyDef), TyInfo(..), MonadTyper
  )

import Nuko.Typer.Infer.Type    (inferTy, inferRealTy)
import Nuko.Typer.Unify         (unifyKind)
import Nuko.Typer.Types         (generalizeWith, Relation(..), TTy(..), TKind(KiStar, KiFun) )

import Nuko.Tree                (Ty, TypeDeclArg(..), Tc, TypeDecl(..), Re)
import Nuko.Names               (mkLocalPath, ConsName, Name, TyName, ValName)

import Relude.Extra (traverseToSnd)
import Data.List (length, unzip)

getRet :: TKind -> TKind
getRet = \case
  KiFun _ ret -> ret
  other -> other

mkTyApp :: TKind -> TTy 'Real -> [(Name TyName, TKind)] -> (TKind, TTy 'Real, [Name TyName])
mkTyApp kind typeTy bindings = do
  let names              = fst <$> bindings
  let (resKind, resType) = foldl' (\(k, a) t -> (getRet k, TyApp k a (TyVar t))) (getRet kind, typeTy) [0 .. length names - 1]
  (resKind, resType, names)

initTypeDecl :: MonadTyper m => TypeDecl Re -> m TyInfo
initTypeDecl decl = do
  indices <- traverse (traverseToSnd newKindHole) decl.tyArgs
  let tyKind = foldr KiFun KiStar (fmap snd indices)
  typeTy <- TyIdent <$> qualifyPath (mkLocalPath decl.tyName)
  let (resKind, resType, _) = mkTyApp tyKind typeTy indices
  unifyKind resKind KiStar
  let resInfo = TyInfo resType typeTy indices IsTyDef
  addTyKind decl.tyName tyKind resInfo
  pure resInfo

inferTypeDecl :: MonadTyper m => TypeDecl Re -> TyInfo -> m (TypeDecl Tc)
inferTypeDecl (TypeDecl name' args arg) tyInfo = do
    decl <- inferBody arg
    -- TODO: put it under scope
    pure (TypeDecl name' args decl)
  where
    inferField :: MonadTyper m => (Name ValName, Ty Re) -> m (Name ValName, TTy 'Virtual)
    inferField (fieldName, ty) = do
      -- TODO: check if FV is empty
      ((inferedTy, kind), fv) <- inferRealTy tyInfo._tyNames ty
      unifyKind kind KiStar
      let names = fst <$> tyInfo._tyNames
      let generalizedTy = generalizeWith names (TyFun inferedTy tyInfo._resultantType) id
      addFieldToEnv name' fieldName (FieldInfo generalizedTy)
      pure (fieldName, generalizedTy)

    inferSumField :: MonadTyper m => (Name ConsName, [Ty Re]) -> m (Name ConsName, [TTy 'Virtual])
    inferSumField (consName, tys) = do
      -- TODO: inferTy generalizes so i have to take some care here.
      (argTys, argKinds) <- unzip <$> traverse (inferTy tyInfo._tyNames) tys

      traverse_ (`unifyKind` KiStar) argKinds
      let genRes f = foldr TyFun f argTys
      let generalizedTy = generalizeWith (fst <$> tyInfo._tyNames) tyInfo._resultantType genRes
      addTy tsConstructors consName (DataConsInfo generalizedTy (length tys))
      pure (consName, argTys)

    inferBody :: MonadTyper m => TypeDeclArg Re -> m (TypeDeclArg Tc)
    inferBody = \case
      TypeSym  _      -> error "Not Implemented Yet!"
      TypeProd fields -> TypeProd <$> traverse inferField fields
      TypeSum fields  -> TypeSum  <$> traverse inferSumField fields

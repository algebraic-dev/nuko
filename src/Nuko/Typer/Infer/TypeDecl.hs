module Nuko.Typer.Infer.TypeDecl (
  initTypeDecl,
  inferTypeDecl
) where

import Nuko.Typer.Env

import Relude                   ((<$>), Traversable (traverse), Foldable (foldr, foldl'), snd, fst, error, traverse_, Num ((-)), ($), id, Int, Maybe (Just))
import Relude.Functor           (Functor (fmap))
import Relude.Applicative       (pure)

import Nuko.Resolver.Tree       ()
import Nuko.Typer.Tree          ()
import Nuko.Typer.Infer.Type    (inferOpenTy)
import Nuko.Typer.Unify         (unifyKind)
import Nuko.Typer.Types         (generalizeWith, Relation(..), TTy(..), TKind(KiStar, KiFun))
import Nuko.Tree                (Ty, TypeDeclArg(..), Tc, TypeDecl(..), Re)
import Nuko.Names               (mkLocalPath, ConsName, Name (nIdent), TyName, ValName)

import Relude.Extra (traverseToSnd)
import Data.List (length, unzip)

getRet :: TKind -> TKind
getRet = \case
  KiFun _ ret -> ret
  other -> other

mkTyApp :: TKind -> TTy 'Real -> [(Name TyName, TKind)] -> (TKind, TTy 'Real, [Name TyName])
mkTyApp kind typeTy bindings = do
    let names              = fst <$> bindings
    let size               = length names - 1
    let (resKind, resType) = foldl' destructTy (getRet kind, typeTy) [0..size]
    (resKind, resType, names)
  where
    destructTy :: (TKind, TTy k) -> Int -> (TKind, TTy k)
    destructTy (k, a) t = (getRet k, TyApp k a (TyVar t))

initTypeDecl :: MonadTyper m => TypeDecl Re -> m TyInfo
initTypeDecl decl = do
  indices <- traverse (traverseToSnd newKindHole) decl.tyArgs

  -- The type kind
  let tyKind = foldr KiFun KiStar (fmap snd indices)

  -- Label type
  typeTy <- TyIdent <$> qualifyPath (mkLocalPath decl.tyName)

  -- The type application that should end with kind equals to *
  let (resKind, resType, _) = mkTyApp tyKind typeTy indices
  unifyKind resKind KiStar
  let resInfo = TyInfo resType decl.tyName indices IsTyDef
  addTyKind decl.tyName tyKind resInfo
  pure resInfo

inferTypeDecl :: MonadTyper m => TypeDecl Re -> TyInfo -> m (TypeDecl Tc)
inferTypeDecl (TypeDecl name' args arg) tyInfo =
    addLocalTypes tyInfo._tyNames $ do
      decl <- inferBody arg
      pure (TypeDecl name' args decl)
  where
    inferField :: MonadTyper m => (Name ValName, Ty Re) -> m (Name ValName, TTy 'Real)
    inferField (fieldName, ty) = do
      (inferedTy, kind) <- inferOpenTy ty
      unifyKind kind KiStar
      -- TODO: check if FV is empty
      let names = fst <$> tyInfo._tyNames
      let realTy = TyFun tyInfo._resultantType inferedTy
      let generalizedTy = generalizeWith names realTy id
      addFieldToEnv name' fieldName (FieldInfo generalizedTy)
      pure (fieldName, realTy)

    inferSumField :: MonadTyper m => (Name ConsName, [Ty Re]) -> m (Name ConsName, [TTy 'Real])
    inferSumField (consName, tys) = do
      -- TODO: inferTy generalizes so i have to take some care here.
      (argTys, argKinds) <- unzip <$> traverse (inferOpenTy) tys

      let names = fst <$> tyInfo._tyNames
      let generalizedTy = generalizeWith names (foldr TyFun tyInfo._resultantType argTys) id
      addTy tsConstructors (Just tyInfo._label.nIdent) consName (DataConsInfo generalizedTy (length tys))

      traverse_ (`unifyKind` KiStar) argKinds
      pure (consName, argTys)

    inferBody :: MonadTyper m => TypeDeclArg Re -> m (TypeDeclArg Tc)
    inferBody = \case
      TypeSym  _      -> error "Not Implemented Yet!"
      TypeProd fields -> TypeProd <$> traverse inferField fields
      TypeSum fields  -> TypeSum  <$> traverse inferSumField fields

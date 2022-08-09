module Nuko.Typer.Infer.TypeDecl (
  initTypeDecl,
  inferTypeDecl
) where

import Nuko.Typer.Env

import Relude
import Relude.Extra          (traverseToSnd)

import Lens.Micro.Platform   (_2, set)
import Nuko.Names            (ConsName, Name (nIdent), TyName, ValName,
                              mkLocalPath)
import Nuko.Report.Range     (getPos)
import Nuko.Resolver.Tree    ()
import Nuko.Tree             (Re, Tc, Ty, TypeDecl (..), TypeDeclArg (..))
import Nuko.Typer.Infer.Type (inferOpenTy)
import Nuko.Typer.Tree       ()
import Nuko.Typer.Types      (Relation (..), TKind (KiFun, KiStar), TTy (..),
                              generalizeNames)
import Nuko.Typer.Unify      (unifyKind)

getRet :: TKind -> TKind
getRet = \case
  KiFun _ ret -> ret
  other       -> other

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
    unifyKind (getPos decl.tyName) resKind KiStar

    tyDef <- getTyDef decl.tyDecl

    let resInfo = TyInfo resType decl.tyName indices tyDef
    addTyKind decl.tyName tyKind resInfo
    pure resInfo
  where
    getTyDef :: MonadTyper m => TypeDeclArg Re -> m TyInfoKind
    getTyDef = \case
      TypeSym  _      -> error "Not Implemented Yet!"
      TypeProd fields -> pure $ IsProdType $ ProdTyInfo (fst <$> fields) []
      TypeSum fields  -> do
        fieldsRes <- traverse (\(n, r) -> (, length r) <$> qualifyTyName (Just decl.tyName.nIdent) n) fields
        pure (IsSumType $ SumTyInfo fieldsRes [])


inferTypeDecl :: MonadTyper m => TypeDecl Re -> TyInfo -> m (TypeDecl Tc)
inferTypeDecl (TypeDecl name' args arg) tyInfo =
    addLocalTypes tyInfo._tyNames $ do
      decl <- inferBody arg
      pure (TypeDecl name' args decl)
  where
    inferField :: MonadTyper m => (Name ValName, Ty Re) -> m (Name ValName, TTy 'Real)
    inferField (fieldName, ty) = do
      (inferedTy, kind) <- inferOpenTy ty
      unifyKind (getPos ty) kind KiStar
      -- TODO: check if FV is empty
      let names = fst <$> tyInfo._tyNames
      let realTy = TyFun inferedTy tyInfo._resultantType
      let generalizedTy = generalizeNames names realTy
      addFieldToEnv name' fieldName (FieldInfo generalizedTy)
      pure (fieldName, realTy)

    inferSumField :: MonadTyper m => (Name ConsName, [Ty Re]) -> m (Name ConsName, [TTy 'Real])
    inferSumField (consName, tys) = do
      -- TODO: inferTy generalizes so i have to take some care here.
      (argTys, argKinds) <- unzip <$> traverse (\n -> (\(t, k) -> (t, (getPos n, k))) <$> inferOpenTy n) tys

      let names = fst <$> tyInfo._tyNames
      let generalizedTy = generalizeNames names (foldr TyFun tyInfo._resultantType argTys)

      path <- qualifyLocal name'
      addTy tsConstructors (Just tyInfo._label.nIdent) consName (generalizedTy, DataConsInfo (length tys) path)

      traverse_ (\(r, k) -> unifyKind r k KiStar) argKinds
      pure (consName, argTys)

    inferBody :: MonadTyper m => TypeDeclArg Re -> m (TypeDeclArg Tc)
    inferBody = \case
      TypeSym  _      -> error "Not Implemented Yet!"
      TypeProd fields -> do
        fieldTys <-  traverse inferField fields
        qualified <- qualifyLocal name'
        updateTyKind qualified (Just . set (_2 . tyKind) (IsProdType $ ProdTyInfo (fst <$> fields) (snd <$> fieldTys)))
        pure $ TypeProd fieldTys
      TypeSum fields  -> do
        fieldsRes <- traverse (\(n, r) -> (, length r) <$> qualifyTyName (Just name'.nIdent) n) fields
        fieldTys <- traverse inferSumField fields
        qualified <- qualifyLocal name'
        updateTyKind qualified (Just . set (_2 . tyKind) (IsSumType $ SumTyInfo fieldsRes (snd <$> toList fieldTys)))
        pure $ TypeSum fieldTys

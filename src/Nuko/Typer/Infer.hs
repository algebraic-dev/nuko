module Nuko.Typer.Infer (
  initTypeDecl,
  inferTypeDecl,
) where

import Relude                (newIORef, snd, fst, Foldable (foldl'))
import Relude.String         (Text)
import Relude.Functor        (Functor(fmap), (<$>))
import Relude.Foldable       (Foldable(foldr), Traversable(traverse), traverse_)
import Relude.Applicative    (Applicative(pure))

import Nuko.Typer.Tree       () -- Just to make the equality between XName Re and XName Tc works
import Nuko.Tree             (TypeDecl(..), Re, Tc, Ty, XName, XTy)
import Nuko.Typer.Env        (MonadTyper, addTyKind, tsFields, addTy, tsConstructors)
import Nuko.Typer.Types      (Hole (Empty), TKind (..), TTy (..), Virtual)
import Nuko.Resolver.Tree    (ReId(text), Path (Local))
import Nuko.Tree.TopLevel    (TypeDeclArg(..))
import Nuko.Typer.Infer.Type (inferTy)
import Nuko.Typer.Unify      (unifyKind)
import Relude.Debug (error)

initTypeDecl :: MonadTyper m => TypeDecl Re -> m (TTy Virtual, [(Text, TKind)])
initTypeDecl decl = do
  bindings <- traverse (\name -> fmap (name.text,) (fmap KiHole (newIORef (Empty name.text 0)))) decl.tyArgs

  let curKind = foldr KiFun KiStar (fmap snd bindings)
  addTyKind decl.tyName.text curKind

  let typeTy = (TyIdent (Local decl.tyName))
  let resultantType = foldl' TyApp typeTy ((\n -> TyIdent (Local n)) <$> decl.tyArgs)

  pure (resultantType, bindings)

inferTypeDecl :: MonadTyper m => TypeDecl Re -> (TTy Virtual, [(Text,TKind)]) -> m (TypeDecl Tc)
inferTypeDecl (TypeDecl name args body) (resultType, bindings) = do
    decl <- inferDecl body
    pure (TypeDecl name args decl)
  where
    inferField :: MonadTyper m => (XName Re, Ty Re) -> m (XName Tc, TTy Virtual)
    inferField (fieldName, ty) = do
      (inferedTy, kind) <- inferTy ty bindings
      unifyKind kind KiStar
      let resTy = TyFun inferedTy resultType
      addTy tsFields fieldName.text resTy
      pure (fieldName, inferedTy)

    inferSumField :: MonadTyper m => (XName Re, [XTy Re]) -> m (XName Tc, [TTy Virtual])
    inferSumField (fieldName, tys) = do
      list <- traverse (\ty -> inferTy ty bindings) tys
      traverse_ (\ty -> unifyKind (snd ty) KiStar) list
      let resTy = foldr TyFun resultType (fst <$> list)
      addTy tsConstructors fieldName.text resTy
      pure (fieldName, (fst <$> list))

    inferDecl :: MonadTyper m => TypeDeclArg Re -> m (TypeDeclArg Tc)
    inferDecl = \case
      TypeSym type'   -> error "Not implemented!"
      TypeProd fields -> TypeProd <$> traverse inferField fields
      TypeSum fields  -> TypeSum  <$> traverse inferSumField fields
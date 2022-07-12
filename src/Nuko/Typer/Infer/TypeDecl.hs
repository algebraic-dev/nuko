module Nuko.Typer.Infer.TypeDecl (
  InitTypeData(..),
  initTypeDecl,
  inferTypeDecl,
  checkTypeSymLoop
) where

import Relude                (newIORef, snd, fst, Foldable (foldl', length), writeIORef, Applicative ((*>)), ($), HashMap, gets, Semigroup ((<>)), zip)
import Relude.String         (Text)
import Relude.Functor        (Functor(fmap), (<$>))
import Relude.Foldable       (Foldable(foldr), Traversable(traverse), traverse_, for_)
import Relude.Applicative    (Applicative(pure))

import Nuko.Typer.Tree       () -- Just to make the equality between XName Re and XName Tc works
import Nuko.Tree             (TypeDecl(..), Re, Tc, Ty, XName, XTy)
import Nuko.Typer.Env        (MonadTyper, addTyKind, addTy, tsConstructors, TyInfo (IsTySyn, IsTyDef), addFieldToEnv, FieldInfo (FieldInfo), TypingEnv (_teCurModule))
import Nuko.Typer.Types      (Hole (..), TKind (..), TTy (..), Virtual, KiHole, generalizeOver)
import Nuko.Resolver.Tree    (ReId(..), Path (..))
import Nuko.Tree.TopLevel    (TypeDeclArg(..))
import Nuko.Typer.Infer.Type (inferTy, findCycle)
import Nuko.Typer.Unify      (unifyKind)

import qualified Data.HashMap.Strict as HashMap

data InitTypeData =
  InitTypeData
  { itBindings  :: [(Text, TKind)]
  , itResTy     :: TTy Virtual
  , itResHole   :: KiHole
  , itResKind   :: TKind
  , itCanonName :: Text
  }

checkTypeSymLoop :: MonadTyper m => [TypeDecl Re] -> m ()
checkTypeSymLoop decls = do
    let filtered = filterDec decls HashMap.empty
    curMod <- gets _teCurModule
    for_ (HashMap.toList filtered) $ \(name, ty) -> findCycle curMod name filtered ty
  where
    filterDec :: [TypeDecl Re] -> HashMap Text (Ty Re) -> HashMap Text (Ty Re)
    filterDec [] m  = m
    filterDec (TypeDecl name _ (TypeSym ty) : xs) m = filterDec xs (HashMap.insert name.text ty m)
    filterDec (_ : xs) m = filterDec xs m

initTypeDecl :: MonadTyper m => TypeDecl Re -> m InitTypeData
initTypeDecl decl = do
  bindings <- traverse (\name -> fmap (name.text,) (fmap KiHole (newIORef (Empty name.text 0)))) decl.tyArgs

  retHole <- newIORef (Empty decl.tyName.text 0)
  let curKind = foldr KiFun (KiHole retHole) (fmap snd bindings)

  tyInfo <-
    case decl.tyDecl of
      TypeSym _   -> pure IsTySyn
      _           -> writeIORef retHole (Filled KiStar) *> pure IsTyDef

  addTyKind decl.tyName.text curKind tyInfo


  let map = HashMap.fromList (zip (fst <$> bindings) [0..])
  let bindingsTys = (\(t, _) -> TyVar (HashMap.lookupDefault 0 t map)) <$> bindings

  curMod <- gets _teCurModule

  let getRet = \case
        KiFun _ b -> b
        other -> other

  let typeTy = (TyIdent (Path curMod decl.tyName decl.tyName.range))
  let (resK, resultType) = foldl' (\(k, a) t -> (getRet k, TyApp k a t)) (getRet curKind, typeTy) bindingsTys

  unifyKind resK KiStar

  let canonName = curMod <> "." <> decl.tyName.text

  pure $ InitTypeData bindings resultType retHole curKind canonName

inferTypeDecl :: MonadTyper m => TypeDecl Re -> InitTypeData -> m (TypeDecl Tc)
inferTypeDecl (TypeDecl name args body) initData = do
    decl <- inferDecl body
    pure (TypeDecl name args decl)
  where

    inferField :: MonadTyper m => (XName Re, Ty Re) -> m (XName Tc, TTy Virtual)
    inferField (fieldName, ty) = do
      (inferedTy, kind) <- inferTy initData.itBindings ty
      unifyKind kind KiStar

      let resTy = TyFun initData.itResTy inferedTy
      let freeVarsSet = fst <$> initData.itBindings
      let generalizedType = generalizeOver freeVarsSet resTy

      addFieldToEnv
        name.text
        fieldName.text
        (FieldInfo generalizedType)

      pure (fieldName, generalizedType)

    inferSumField :: MonadTyper m => (XName Re, [XTy Re]) -> m (XName Tc, [TTy Virtual])
    inferSumField (fieldName, tys) = do
      argTys <- traverse (inferTy initData.itBindings) tys

      traverse_ (\ty -> unifyKind (snd ty) KiStar) argTys
      let resTy  = foldr TyFun initData.itResTy (fst <$> argTys)

      let texts = fst <$> initData.itBindings
      let resTy' = generalizeOver texts resTy

      addTy tsConstructors (name.text <> "." <> fieldName.text) (resTy', length argTys)

      pure (fieldName, (fst <$> argTys))

    inferDecl :: MonadTyper m => TypeDeclArg Re -> m (TypeDeclArg Tc)
    inferDecl = \case
      TypeSym type'   -> do
        (resTy, resKind) <- inferTy initData.itBindings type'
        writeIORef initData.itResHole (Filled resKind)
        pure (TypeSym resTy)
      TypeProd fields ->
        TypeProd <$> traverse inferField fields
      TypeSum fields  ->
        TypeSum  <$> traverse inferSumField fields

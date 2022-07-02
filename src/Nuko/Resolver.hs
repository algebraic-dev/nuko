module Nuko.Resolver (
  resolveImport,
  resolveTypeDecl,
  resolveLit,
  resolvePat,
  resolveProgram,
  resolveLetDecl,
  resolveExpr
) where

import Nuko.Tree.TopLevel
import Nuko.Resolver.Tree
import Nuko.Tree.Expr
import Nuko.Syntax.Range          (Range(..))
import Nuko.Syntax.Tree           (Name(..))
import Nuko.Resolver.Types        (MonadResolver, makePath, addGlobal, findInSpace, resolvePath, resolveName)
import Nuko.Resolver.Environment  (LocalNS (..), NameSpace(..), addModule, Visibility (..), openName, getPublicNames, scopeLocals, addLocal, scopeNameSpace)
import Nuko.Resolver.Error        (ResolveError(..))
import Nuko.Resolver.Occourence   (OccName(..), NameKind (..))
import Nuko.Tree                  (Nm, Re)
import Nuko.Utils                 (terminate)

import Relude.Applicative         (Applicative(..))
import Relude.Monoid              ((<>))
import Relude.Monad               (Maybe(..), Either(..), gets, StateT (runStateT), modify)
import Relude.String              (Text)
import Relude.List                (NonEmpty ((:|)))
import Relude                     (traverse_, One (one), Functor (fmap), ($), (<$>), Traversable (traverse), (.), MonadTrans (lift), ($>))

import Data.HashMap.Strict        (HashMap)
import Control.Monad.Import       (ImportErrorKind(..), MonadImport (importIn))

import qualified Data.HashMap.Strict as HashMap

-- Init for mutual things.

initLetDecl :: MonadResolver m => LetDecl Nm -> m ()
initLetDecl (LetDecl name _ _ _ _) = addGlobal name.range (OccName name.text VarName) Public

initTyDecl :: MonadResolver m => TypeDecl Nm -> m ()
initTyDecl (TypeDecl name _ _) = addGlobal name.range (OccName name.text TyName) Public

initProgram :: MonadResolver m => Program Nm -> m ()
initProgram (Program tyDecls letDecls _ _) = do
  traverse_ initTyDecl tyDecls
  traverse_ initLetDecl letDecls

-- Resolution of names

resolveImport :: MonadResolver m => Import Nm -> m ()
resolveImport decl = do
    let (name, range) = makePath decl.path
    space <- importModule name range
    case decl.modifier of
      Just res -> resolveModifier space res
      Nothing  -> addModule name space
  where
    importModule :: MonadResolver m => Text -> Range -> m NameSpace
    importModule name range = do
      res <- importIn name
      case res of
        Right res'      -> pure res'
        Left CannotFind -> terminate (CannotFindModule name range)
        Left Cyclic     -> terminate (CyclicImport name range)

    resolveByCase :: MonadResolver m => NameSpace -> ImportDepsKind Nm -> m OccName
    resolveByCase space = \case
      ImpDepLower name -> findInSpace space name.range name.text (one VarName)
      ImpDepUpper name -> findInSpace space name.range name.text (TyName :| [ConsName])

    resolveDeps :: MonadResolver m => NameSpace -> ImportDeps Nm -> m ()
    resolveDeps space (ImportDeps path name) = do
      (OccName occ kind) <- resolveByCase space path
      case name of
        Just alias -> openName (OccName alias.text kind) space._modName occ
        Nothing    -> openName (OccName occ kind) space._modName occ

    resolveModifier :: MonadResolver m => NameSpace -> ImportModifier Nm -> m ()
    resolveModifier space = \case
      ImpAs as     -> addModule as.text space
      ImpList imps -> traverse_ (resolveDeps space) imps
      ImpStar      -> traverse_ (\occ -> openName occ space._modName occ.name) (getPublicNames space._names)

toReId :: Name -> ReId
toReId (Name text r) = ReId text r

resolveTypeDecl :: MonadResolver m => TypeDecl Nm -> m (TypeDecl Re)
resolveTypeDecl (TypeDecl name args decl) = do
    curName <- gets (_modName . _currentNamespace)
    -- Creates a namespace for the type and scope it's local type variables.
    scopeNameSpace (curName <> "." <> name.text) $ scopeLocals $ do
      traverse_ (\name' -> addLocal name'.range (OccName name'.text TyName)) args
      decl' <- resolveDecl decl
      pure (TypeDecl (toReId name) (fmap toReId args) decl')
  where
    resolveField :: MonadResolver m => (XName Nm, Ty Nm) -> m (ReId, Ty Re)
    resolveField (name', ty) = do
      addGlobal name.range (OccName name'.text FieldName) Public
      (toReId name, ) <$> resolveType ty

    resolveSumField :: MonadResolver m => (XName Nm, [Ty Nm]) -> m (ReId, [Ty Re])
    resolveSumField (name', ty) = do
      addGlobal name.range (OccName name'.text ConsName) Public
      (toReId name, ) <$> traverse resolveType ty

    resolveDecl :: MonadResolver m => TypeDeclArg Nm -> m (TypeDeclArg Re)
    resolveDecl = \case
      TypeSym type'   -> TypeSym  <$> resolveType type'
      TypeProd fields -> TypeProd <$> traverse resolveField fields
      TypeSum fields  -> TypeSum <$> traverse resolveSumField fields

resolveType :: MonadResolver m => Ty Nm -> m (Ty Re)
resolveType = \case
  TId path ext                -> TId    <$> resolvePath TyName path <*> pure ext
  TPoly (Name text range) ext -> TId    <$> resolveName TyName range text <*> pure ext
  TCons path ty ext           -> TCons  <$> resolvePath TyName path <*> traverse resolveType ty <*> pure ext
  TArrow from to ext          -> TArrow <$> resolveType from <*> resolveType to <*> pure ext
  TForall name ty ext         -> scopeLocals $ do
    addLocal name.range (OccName name.text TyName)
    TForall (toReId name) <$> resolveType ty <*> pure ext

resolveLit :: MonadResolver m => Literal Nm -> m (Literal Re)
resolveLit = \case
  LStr str ext -> pure (LStr str ext)
  LInt int ext -> pure (LInt int ext)

resolvePat :: MonadResolver m => Pat Nm -> m (Pat Re)
resolvePat pat = do
    (res, bindings) <- runStateT (resolvePat' pat) HashMap.empty
    traverse_ (\(name, range) -> addLocal range (OccName name VarName)) (HashMap.toList bindings)
    pure res
  where
    resolvePat' :: MonadResolver m => Pat Nm -> StateT (HashMap Text Range) m (Pat Re)
    resolvePat' = \case
      PWild ext           -> pure $ PWild ext
      PCons path pats ext -> PCons <$> lift (resolvePath ConsName path) <*> traverse resolvePat' pats <*> pure ext
      PLit lit ext        -> PLit  <$> lift (resolveLit lit) <*> pure ext
      PAnn pat' ty ext    -> PAnn  <$> resolvePat' pat'      <*> lift (resolveType ty) <*> pure ext
      PId name ext        -> do
        exists <- gets (HashMap.member name.text)
        if exists
          then terminate (AlreadyExistsPat name.text name.range)
          else modify (HashMap.insert name.text name.range) $> PId (toReId name) ext

resolveExpr :: MonadResolver m => Expr Nm -> m (Expr Re)
resolveExpr = \case
    Lit lit ext           -> Lit <$> resolveLit lit <*> pure ext
    Lam pat expr ext      -> scopeLocals (Lam <$> resolvePat pat <*> resolveExpr expr <*> pure ext)
    App expr args ext     -> App <$> resolveExpr expr <*> traverse resolveExpr args <*> pure ext
    Lower path ext        -> Lower <$> resolvePath VarName path <*> pure ext
    Upper path ext        -> Upper <$> resolvePath ConsName path <*> pure ext
    Field expr field ext  -> Field <$> resolveExpr expr <*> pure (toReId field) <*> pure ext
    If cond if' else' ext -> If <$> resolveExpr cond <*> resolveExpr if' <*> traverse resolveExpr else' <*> pure ext
    Match scrut cases ext -> Match <$> resolveExpr scrut <*> traverse resolveCase cases <*> pure ext
    Ann expr ty ext       -> Ann <$> resolveExpr expr <*> resolveType ty <*> pure ext
    Block block ext       -> scopeLocals $ Block <$> resolveBlock block <*> pure ext
  where
    resolveCase :: MonadResolver m => (Pat Nm, Expr Nm) -> m (Pat Re, Expr Re)
    resolveCase (pat, expr) = scopeLocals ((,) <$> resolvePat pat <*> resolveExpr expr)

    resolveBlock :: MonadResolver m => Block Nm -> m (Block Re)
    resolveBlock = \case
       BlVar (Var pat val ext) rest -> BlVar  <$> (Var <$> resolvePat pat <*> resolveExpr val <*> pure ext) <*> resolveBlock rest
       BlBind expr rest             -> BlBind <$> resolveExpr expr <*> resolveBlock rest
       BlEnd expr                   -> BlEnd  <$> resolveExpr expr

resolveLetDecl :: MonadResolver m => LetDecl Nm -> m (LetDecl Re)
resolveLetDecl (LetDecl name args body ret ext) =
  LetDecl     (toReId name)
          <$> traverse (\(name', ty) -> (,) (toReId name') <$> resolveType ty) args
          <*> resolveExpr body
          <*> traverse resolveType ret
          <*> pure ext

resolveProgram :: MonadResolver m => Program Nm -> m (Program Re)
resolveProgram program@(Program tyDecls letDecls impDecls ext) = do
  traverse_ resolveImport impDecls
  initProgram program
  letDecls' <- traverse resolveLetDecl letDecls
  tyDecls'  <- traverse resolveTypeDecl tyDecls
  pure (Program tyDecls' letDecls' [] ext)

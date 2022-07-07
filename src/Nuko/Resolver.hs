module Nuko.Resolver (
  resolveImport,
  resolveTypeDecl,
  resolveLit,
  resolvePat,
  resolveProgram,
  resolveLetDecl,
  resolveExpr,
  initProgram,
) where

import Nuko.Tree.TopLevel
import Nuko.Tree.Expr
import Nuko.Resolver.Environment
import Nuko.Resolver.Tree         (ReId(ReId), Path (..))
import Nuko.Resolver.Types        (MonadResolver, makePath, addGlobal, findInSpace, resolvePath, resolveName)
import Nuko.Resolver.Error        (ResolveError(..))
import Nuko.Resolver.Occourence   (OccName(..), NameKind (..))
import Nuko.Syntax.Range          (Range(..))
import Nuko.Syntax.Tree           (Name(..))
import Nuko.Tree                  (Nm, Re)
import Nuko.Utils                 (terminate)

import Relude.Applicative         (Applicative(..))
import Relude.Functor             (Functor (fmap), (<$>))
import Relude.Monoid              ((<>))
import Relude.Monad               (Maybe(..), Either(..), gets, StateT (runStateT), modify, MonadTrans (lift))
import Relude.String              (Text)
import Relude.Container           (One (one))
import Relude.List                (NonEmpty ((:|)))
import Relude                     (traverse_, ($), Traversable (traverse), (.), ($>), fst, when, Ord ((>)), for_)

import Data.HashMap.Strict        (HashMap)
import Control.Monad.Import       (ImportErrorKind(..), MonadImport (..))

import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (groupAllWith)
import qualified Data.List.NonEmpty as NonEmpty

-- Init for mutual things.

initLetDecl :: MonadResolver m => LetDecl Nm -> m ()
initLetDecl (LetDecl name' _ _ _ _) = addGlobal name'.range (OccName name'.text VarName) Public

initTyDecl :: MonadResolver m => TypeDecl Nm -> m ()
initTyDecl (TypeDecl name' _ decl) = do
    curName <- gets (_modName . _currentNamespace)
    let moduleName = curName <> "." <> name'.text

    openName (OccName name'.text TyName) moduleName name'.text

    scopeNameSpace moduleName $ do
      -- Adds twice because it will be inside the module
      addGlobal name'.range (OccName name'.text TyName) Public
      initFields decl
  where
    initFields :: MonadResolver m => TypeDeclArg Nm -> m ()
    initFields = \case
      TypeSym _       -> pure ()
      TypeProd fields -> traverse_ (\field -> addGlobal field.range (OccName field.text FieldName) Public) (fmap fst fields)
      TypeSum fields  -> traverse_ (\field -> addGlobal field.range (OccName field.text ConsName) Public) (fmap fst fields)

initProgram :: MonadResolver m => Program Nm -> m ()
initProgram (Program tyDecls' letDecls' _ _) = do
  traverse_ initTyDecl tyDecls'
  traverse_ initLetDecl letDecls'

-- Resolution of names

resolveImport :: MonadResolver m => Import Nm -> m ()
resolveImport decl = do
    let (name', loc) = makePath decl.path
    space <- importModule name' loc
    case decl.modifier of
      Just res -> resolveModifier space res
      Nothing  -> addModule name' space
  where
    importModule :: MonadResolver m => Text -> Range -> m NameSpace
    importModule name' loc = do
      res <- importIn name'
      case res of
        Right res'      -> pure res'
        Left CannotFind -> terminate (CannotFindModule name' loc)
        Left Cyclic     -> terminate (CyclicImport name' loc)

    resolveByCase :: MonadResolver m => NameSpace -> ImportDepsKind Nm -> m OccName
    resolveByCase space = \case
      ImpDepLower name' -> findInSpace space name'.range name'.text (one VarName)
      ImpDepUpper name' -> findInSpace space name'.range name'.text (TyName :| [ConsName])

    resolveDeps :: MonadResolver m => NameSpace -> ImportDeps Nm -> m ()
    resolveDeps space (ImportDeps path' name') = do
      (OccName occ kind') <- resolveByCase space path'
      case name' of
        Just alias -> openName (OccName alias.text kind') space._modName occ
        Nothing    -> openName (OccName occ kind') space._modName occ

    resolveModifier :: MonadResolver m => NameSpace -> ImportModifier Nm -> m ()
    resolveModifier space = \case
      ImpAs alias  -> addModule alias.text space
      ImpList imps -> traverse_ (resolveDeps space) imps
      ImpStar      -> traverse_ (\occ -> openName occ space._modName occ.occName) (getPublicNames space._names)

toReId :: Name -> ReId
toReId (Name t r) = ReId t r

resolveTypeDecl :: MonadResolver m => TypeDecl Nm -> m (TypeDecl Re)
resolveTypeDecl (TypeDecl name' args decl) = do
    -- Creates a namespace for the type and scope it's local type variables.
    scopeLocals $ do

      -- Checking for duplicated
      let grouped = groupAllWith (\a -> a.text) args
      for_ grouped $ \group ->
        when (NonEmpty.length group > 1)
             (terminate (ConflictingTypes ((\n -> (n.text, n.range)) <$> group)))

      traverse_ (\arg -> addLocal arg.range (OccName arg.text TyName)) args
      decl' <- resolveDecl decl
      pure (TypeDecl (toReId name') (fmap toReId args) decl')
  where
    resolveField :: MonadResolver m => (XName Nm, Ty Nm) -> m (ReId, Ty Re)
    resolveField (fieldName, ty) = (toReId fieldName, ) <$> resolveType ty

    resolveSumField :: MonadResolver m => (XName Nm, [Ty Nm]) -> m (ReId, [Ty Re])
    resolveSumField (fieldName, ty) = (toReId fieldName, ) <$> traverse resolveType ty

    resolveDecl :: MonadResolver m => TypeDeclArg Nm -> m (TypeDeclArg Re)
    resolveDecl = \case
      TypeSym type'   -> TypeSym  <$> resolveType type'
      TypeProd fields -> TypeProd <$> traverse resolveField fields
      TypeSum fields  -> TypeSum <$> traverse resolveSumField fields

resolveType :: MonadResolver m => Ty Nm -> m (Ty Re)
resolveType = \case
  TPoly (Name t r) ext'         -> do
    res <- resolveName TyName r t 
    case res of
      Local res' -> pure (TPoly res' ext')
      _          -> pure (TId res ext') -- Probably impossible
  TId path' ext'                -> TId    <$> resolvePath TyName path' <*> pure ext'
  TApp ty' ty ext'              -> TApp   <$> resolveType ty' <*> traverse resolveType ty <*> pure ext'
  TArrow from to ext'           -> TArrow <$> resolveType from <*> resolveType to <*> pure ext'
  TForall binder ty ext'        -> scopeLocals $ do
    addLocal binder.range (OccName binder.text TyName)
    TForall (toReId binder) <$> resolveType ty <*> pure ext'

resolveLit :: MonadResolver m => Literal Nm -> m (Literal Re)
resolveLit = \case
  LStr str ext' -> pure (LStr str ext')
  LInt int ext' -> pure (LInt int ext')

resolvePat :: MonadResolver m => Pat Nm -> m (Pat Re)
resolvePat pat' = do
    (res, bindings) <- runStateT (resolvePat' pat') HashMap.empty
    traverse_ (\(name', range') -> addLocal range' (OccName name' VarName)) (HashMap.toList bindings)
    pure res
  where
    resolvePat' :: MonadResolver m => Pat Nm -> StateT (HashMap Text Range) m (Pat Re)
    resolvePat' = \case
      PWild ext'            -> pure $ PWild ext'
      PCons path' pats ext' -> PCons <$> lift (resolvePath ConsName path') <*> traverse resolvePat' pats <*> pure ext'
      PLit lit ext'         -> PLit   <$> lift (resolveLit lit) <*> pure ext'
      PAnn pat'' ty ext'    -> PAnn  <$> resolvePat' pat''      <*> lift (resolveType ty) <*> pure ext'
      PId name' ext'         -> do
        exists <- gets (HashMap.member name'.text)
        if exists
          then terminate (AlreadyExistsPat name'.text name'.range)
          else modify (HashMap.insert name'.text name'.range) $> PId (toReId name') ext'

resolveExpr :: MonadResolver m => Expr Nm -> m (Expr Re)
resolveExpr = \case
    Lit lit ext'           -> Lit <$> resolveLit lit <*> pure ext'
    Lam pat' expr ext'     -> scopeLocals (Lam <$> resolvePat pat' <*> resolveExpr expr <*> pure ext')
    App expr args ext'     -> App <$> resolveExpr expr <*> traverse resolveExpr args <*> pure ext'
    Lower path' ext'       -> Lower <$> resolvePath VarName path' <*> pure ext'
    Upper path' ext'       -> Upper <$> resolvePath ConsName path' <*> pure ext'
    Field expr field ext'  -> Field <$> resolveExpr expr <*> pure (toReId field) <*> pure ext'
    If cond if' else' ext' -> If <$> resolveExpr cond <*> resolveExpr if' <*> traverse resolveExpr else' <*> pure ext'
    Match scrut cases ext' -> Match <$> resolveExpr scrut <*> traverse resolveCase cases <*> pure ext'
    Ann expr ty ext'       -> Ann <$> resolveExpr expr <*> resolveType ty <*> pure ext'
    Block block ext'       -> scopeLocals $ Block <$> resolveBlock block <*> pure ext'
  where
    resolveCase :: MonadResolver m => (Pat Nm, Expr Nm) -> m (Pat Re, Expr Re)
    resolveCase (pat', expr) = scopeLocals ((,) <$> resolvePat pat' <*> resolveExpr expr)

    resolveBlock :: MonadResolver m => Block Nm -> m (Block Re)
    resolveBlock = \case
       BlVar (Var pat' var ext') rest -> BlVar  <$> (Var <$> resolvePat pat' <*> resolveExpr var <*> pure ext') <*> resolveBlock rest
       BlBind expr rest             -> BlBind <$> resolveExpr expr <*> resolveBlock rest
       BlEnd expr                   -> BlEnd  <$> resolveExpr expr

resolveLetDecl :: MonadResolver m => LetDecl Nm -> m (LetDecl Re)
resolveLetDecl (LetDecl name' args body ret ext') =
  scopeLocals $
      LetDecl (toReId name')
          <$> traverse (\(name'', ty) -> ((,) (toReId name'') <$> resolveType ty) <* addLocal name''.range (OccName name''.text VarName)) args
          <*> resolveExpr body
          <*> resolveType ret
          <*> pure ext'

resolveProgram :: MonadResolver m => Program Nm -> m (Program Re)
resolveProgram (Program tyDecls' letDecls' impDecls' ext') = do
  traverse_ resolveImport impDecls'
  letDecls'' <- traverse resolveLetDecl letDecls'
  tyDecls''  <- traverse resolveTypeDecl tyDecls'
  pure (Program tyDecls'' letDecls'' [] ext')
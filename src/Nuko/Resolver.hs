module Nuko.Resolver (
  initProgram,
  resolveProgram
) where

import Nuko.Tree
import Nuko.Names
import Nuko.Resolver.Env
import Nuko.Resolver.Path         (resolvePath,resolveConsOrTy,resolveInNameSpace,getPublicLabels, useLocalPath)
import Nuko.Resolver.Error        (mkErr, ResolveErrorReason (..))
import Nuko.Utils                 (terminate)
import Nuko.Syntax.Tree           ()
import Nuko.Resolver.Tree         ()

import Relude.Applicative         (Applicative(..))
import Relude.Functor             (Functor (fmap), (<$>))
import Relude.Monad               (Maybe(..), Either(..), gets, StateT (runStateT), modify, MonadTrans (lift), Monad ((>>=)))
import Relude.Container           (HashSet)
import Relude.List                (NonEmpty ((:|)))
import Relude                     (traverse_, ($), Traversable (traverse), (.), ($>), fst, when, Ord ((>)), for_)

import Data.List.NonEmpty         (groupAllWith)
import Control.Monad.Query       (MonadQuery (..))

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.HashSet as HashSet

initProgram :: MonadResolver m => Program Nm -> m ()
initProgram (Program tyDecls' letDecls' _ _) = do
  traverse_ initTyDecl tyDecls'
  traverse_ initLetDecl letDecls'

resolveProgram :: MonadResolver m => Program Nm -> m (Program Re)
resolveProgram (Program tyDecls' letDecls' impDecls' ext') = do
  traverse_ resolveImport impDecls'
  letDecls'' <- traverse resolveLetDecl letDecls'
  tyDecls''  <- traverse resolveTypeDecl tyDecls'
  pure (Program tyDecls'' letDecls'' [] ext')

resolveImport :: MonadResolver m => Import Nm -> m ()
resolveImport decl = do
    space <- importModule decl.path
    case decl.modifier of
      Just res -> resolveModifier space res
      Nothing  -> addModule decl.path space
  where
    importModule :: MonadResolver m => ModName -> m NameSpace
    importModule mod' = do
      res <- query (GetModule mod')
      case res of
        Right res'-> pure res'
        Left Cyclic -> terminate (mkErr $ CyclicImport mod')
        Left CannotFind -> terminate (mkErr $ CannotFindModule mod')

    resolveByCase :: MonadResolver m => NameSpace -> ImportDepsKind Nm -> m (Qualified Label)
    resolveByCase ns = \case
      ImpDepLower name' -> (Label <$>) <$> resolveInNameSpace ns (mkName ValName name' Untouched)
      ImpDepUpper name' -> resolveConsOrTy ns name'

    resolveDeps :: MonadResolver m => NameSpace -> ImportDeps Nm -> m ()
    resolveDeps space (ImportDeps path' name') = do
      resPath <- resolveByCase space path'
      case name' of
        Just alias -> openName (mkLabel TyName alias Untouched) resPath
        Nothing -> openName resPath.qInfo resPath

    resolveModifier :: MonadResolver m => NameSpace -> ImportModifier Nm -> m ()
    resolveModifier space = \case
      ImpAs alias -> addModule (mkModName (alias :| [])) space
      ImpList imps -> traverse_ (resolveDeps space) imps
      ImpStar -> traverse_ (\occ -> openName occ (mkQualifiedWithPos space._modName occ)) (getPublicLabels space)

initTyDecl :: MonadResolver m => TypeDecl Nm -> m ()
initTyDecl (TypeDecl name' _ decl) = do
    curName <- gets (_modName . _currentNamespace)
    addGlobal name' Public
    newNamespace (addSegments curName [name'.nIdent]) (initFields decl)
  where
    initFields :: MonadResolver m => TypeDeclArg Nm -> m ()
    initFields = \case
      TypeSym _  -> pure ()
      TypeProd fields -> traverse_ (`addGlobal` Public) (fmap fst fields)
      TypeSum fields -> traverse_ (`addGlobal` Public) (fmap fst fields)

resolveTypeDecl :: MonadResolver m => TypeDecl Nm -> m (TypeDecl Re)
resolveTypeDecl (TypeDecl name' args decl) = do
  newScope $ do
    let argumentGroups = groupAllWith (iText . nIdent) args
    for_ argumentGroups $ \group -> when (NonEmpty.length group > 1) (terminate (mkErr $ ConflictingTypes group))
    newArgs <- traverse newLocal args
    newDecl <- resolveDecl decl
    pure (TypeDecl name' newArgs newDecl)
  where
    resolveField :: MonadResolver m => (Name ValName, Ty Nm) -> m (Name ValName, Ty Re)
    resolveField (fieldName, ty) = (fieldName, ) <$> resolveType ty

    resolveSumField :: MonadResolver m => (Name ConsName, [Ty Nm]) -> m (Name ConsName, [Ty Re])
    resolveSumField (fieldName, ty) = (fieldName, ) <$> traverse resolveType ty

    resolveDecl :: MonadResolver m => TypeDeclArg Nm -> m (TypeDeclArg Re)
    resolveDecl = \case
      TypeSym type' -> TypeSym  <$> resolveType type'
      TypeProd fields -> TypeProd <$> traverse resolveField fields
      TypeSum fields -> TypeSum  <$> traverse resolveSumField fields

initLetDecl :: MonadResolver m => LetDecl Nm -> m ()
initLetDecl (LetDecl name' _ _ _ _) = addGlobal name' Public

resolveLetDecl :: MonadResolver m => LetDecl Nm -> m (LetDecl Re)
resolveLetDecl (LetDecl name' args body ret ext') =
  newScope $
      LetDecl name'
          <$> traverse (\(name'', ty) -> ((,) name'' <$> resolveType ty) <* newLocal name'') args
          <*> resolveExpr body
          <*> resolveType ret
          <*> pure ext'

resolveLit :: MonadResolver m => Literal Nm -> m (Literal Re)
resolveLit = \case
  LStr str ext' -> pure (LStr str ext')
  LInt int ext' -> pure (LInt int ext')

resolveType :: MonadResolver m => Ty Nm -> m (Ty Re)
resolveType = \case
  TForall binder ty ext' -> newScope $ newLocal binder >>= \name -> TForall name <$> resolveType ty <*> pure ext'
  TId path' ext' -> TId <$> resolvePath path' <*> pure ext'
  TApp ty' ty ext' -> TApp <$> resolveType ty' <*> traverse resolveType ty <*> pure ext'
  TArrow from to ext' -> TArrow <$> resolveType from <*> resolveType to <*> pure ext'
  TPoly name ext' -> do
    result <- useLocalPath name
    case result of
      Just res -> pure (TPoly res ext')
      Nothing  -> pure (TPoly name ext')

resolvePat :: MonadResolver m => Pat Nm -> m (Pat Re)
resolvePat pat' = do
    (res, _) <- runStateT (resolvePat' pat') HashSet.empty
    pure res
  where
    resolvePat' :: MonadResolver m => Pat Nm -> StateT (HashSet (Name ValName)) m (Pat Re)
    resolvePat' = \case
      PWild ext' -> pure $ PWild ext'
      PCons path' pats ext' -> PCons <$> lift (resolvePath path') <*> traverse resolvePat' pats <*> pure ext'
      PLit lit ext' -> PLit <$> lift (resolveLit lit) <*> pure ext'
      PAnn pat'' ty ext' -> PAnn <$> resolvePat' pat'' <*> lift (resolveType ty) <*> pure ext'
      PId name' ext' -> do
        exists <- gets (HashSet.member name')
        if exists
          then terminate (mkErr $ AlreadyExistsPat name')
          else do
            result <- lift (newLocal name')
            modify (HashSet.insert result) $> PId result ext'

resolveExpr :: MonadResolver m => Expr Nm -> m (Expr Re)
resolveExpr = \case
    Lower path' ext' -> Lower <$> resolvePath path' <*> pure ext'
    Lit lit ext' -> Lit <$> resolveLit lit <*> pure ext'
    Lam pat' expr ext' -> newScope (Lam <$> resolvePat pat' <*> resolveExpr expr <*> pure ext')
    App expr args ext' -> App <$> resolveExpr expr <*> traverse resolveExpr args <*> pure ext'
    Upper path' ext' -> Upper <$> resolvePath path' <*> pure ext'
    Field expr field ext' -> Field <$> resolveExpr expr <*> pure field <*> pure ext'
    If cond if' else' ext' -> If <$> resolveExpr cond <*> resolveExpr if' <*> resolveExpr else' <*> pure ext'
    Match scrut cases ext' -> Match <$> resolveExpr scrut <*> traverse resolveCase cases <*> pure ext'
    Ann expr ty ext' -> Ann <$> resolveExpr expr <*> resolveType ty <*> pure ext'
    Block block ext' -> newScope (Block <$> resolveBlock block <*> pure ext')
  where
    resolveCase :: MonadResolver m => (Pat Nm, Expr Nm) -> m (Pat Re, Expr Re)
    resolveCase (pat', expr) = newScope ((,) <$> resolvePat pat' <*> resolveExpr expr)

    resolveBlock :: MonadResolver m => Block Nm -> m (Block Re)
    resolveBlock = \case
       BlBind expr rest -> BlBind <$> resolveExpr expr <*> resolveBlock rest
       BlEnd expr -> BlEnd <$> resolveExpr expr
       BlVar (Var pat' var ext') rest -> do
         resExpr <- resolveExpr var
         resPat  <- resolvePat pat'
         resBlock <- resolveBlock rest
         pure (BlVar (Var resPat resExpr ext') resBlock)
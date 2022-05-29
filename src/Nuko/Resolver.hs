module Nuko.Resolver (
    Env(..),
    Module(..),
    Resolution(..),
    MonadInit,
    MonadResolve,
    mergeResolution,
    resolveProgram,
    initProgram,
    initToResolve
) where

import Nuko.Resolver.Support
import Nuko.Tree.Expr
import Nuko.Tree.TopLevel
import Control.Monad.State        (void, gets, modify, StateT, MonadState(get, put), execStateT)
import Control.Monad.Except       (MonadError (throwError))
import Control.Monad.Reader       (asks, MonadReader (local))
import Data.HashMap.Strict        (HashMap)
import Data.Text                  (Text)
import Data.List.NonEmpty         (NonEmpty ((:|)))
import Data.Maybe                 (catMaybes)
import Nuko.Syntax.Range          (Range(..))
import Nuko.Resolver.Error        (ResolutionError (..))
import Lens.Micro.Platform        (Lens', view, over, _1, _2, set)
import Nuko.Syntax.Ast            (Normal)
import Data.Void                  (absurd)
import Data.Foldable              (traverse_)
import Nuko.Resolver.Resolved     (Resolved, ResPath (ResPath))
import Data.HashSet               (HashSet)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.HashSet as HashSet

type MonadResolve m =
  ( MonadReader Env m
  , MonadState  Module m
  , MonadError  ResolutionError m
  , MonadImport Module m
  )

type MonadInit m =
  ( MonadState  (Env, Module) m
  , MonadError  ResolutionError m
  , MonadImport Module m
  )

-- Paths

getName :: Name Normal -> Text
getName (Name t _) = t
getName (NaExt x)  = absurd x

getNameResolved :: Name Resolved -> Text
getNameResolved (Name t _) = t
getNameResolved (NaExt x)  = absurd x

normalPath :: Path Normal -> Text
normalPath (Path path final _) = Text.intercalate "." (map getName $ path <> [final])
normalPath (PaExt ab)          = absurd ab

resolved :: Text -> Resolution
resolved = Resolution . (:| [])

-- Initialization

getModName :: MonadInit m => m Text
getModName = gets (_moduleName . snd)

openMod :: MonadInit m => Module -> m ()
openMod mod' = modify $ over (_1 . openedModules) $ HashMap.insert mod'._moduleName mod'

localMod :: MonadState (c, b) m => b -> m a -> m a
localMod mod' op = do
  old <- get
  put (fst old, mod') *> op <* modify (set _2 (snd old))

getModule :: (MonadInit m) => Range -> Text -> m Module
getModule range name = do
  result <- importModule name
  case result of
    NotFound      -> throwError (ModuleNotFound range (name <> ">"))
    Succeded mod' -> pure mod'

initImport :: MonadInit m => Import Normal -> m ()
initImport (Import (PaExt x) _ _)                 = absurd x
initImport (Import mod'@(Path p f range) alias _) = do
  module'  <- getModule range (normalPath mod')
  let aliasName = maybe (last (p ++ [f])) id alias
  modify $ over (_1 . aliasedModules) $ HashMap.insert (getName aliasName) (normalPath mod')
  modify $ over (_1 . importedModules) $ HashMap.insert module'._moduleName module'

initLetDecl :: MonadInit m => LetDecl Normal -> m ()
initLetDecl (LetDecl name _ _ _ _) = do
  name' <- getModName
  modify $ over (_2 . valueDecls) $ HashMap.insert (getName name) (resolved name')

initTyArgs :: MonadInit m => TypeDeclArg Normal -> m ()
initTyArgs = \case
    TypeSym _          -> pure ()
    TypeProd fields    -> addFields fieldDecls (map (getName . fst) fields)
    TypeSum  (x :| xs) -> addFields consDecls  (map (getName . fst) (x : xs))
  where
    addFields :: MonadInit m => Lens' Module (HashMap Text Resolution) -> [Text] -> m ()
    addFields field keys = do
      name' <- getModName
      res <- gets (view $ _2 . field)
      let newRes = foldr (\key -> HashMap.insert key (resolved name')) res keys
      modify (set (_2 . field) newRes)

initTyDecl :: MonadInit m => TypeDecl Normal -> m ()
initTyDecl (TypeDecl name _ decl) = do
    name'         <- getModName
    modify $ over (_2 . tyDecls) $ HashMap.insert (getName name) (resolved name')
    modName       <- gets $ view (_2 . moduleName)
    let newModName = appendToPre modName (getName name)
    typeModule    <- localMod (emptyMod newModName) (initTyArgs decl *> gets snd)
    modify $ over (_1 . aliasedModules) $ HashMap.insert (getName name) newModName
    void $ addModule newModName typeModule
  where
    appendToPre :: Text -> Text -> Text
    appendToPre "" x = x
    appendToPre x y  = x <> "." <> y


initProgram :: MonadInit m => Module -> Program Normal -> m ()
initProgram prelude (Program tyDeps letDeps impDeps _) = do
  openMod prelude
  traverse_ initImport impDeps
  traverse_ initTyDecl tyDeps
  traverse_ initLetDecl letDeps
  res <- gets snd
  modify (set (_1 . currentModule) res)

-- Helpers

mergeResolution :: Resolution -> Resolution -> Resolution
mergeResolution (Resolution x) (Resolution y) = Resolution $ NonEmpty.nub $ (x <> y)

getOpened :: MonadResolve m => Lens' Module (HashMap Text Resolution) -> Range -> Text -> m Text
getOpened lens range key = do
    cached <- gets (view lens)
    flip resolveRes (HashMap.lookup key cached) $ do
      opened  <- asks _openedModules
      current <- asks _currentModule
      name    <- resolveRes (throwError (VariableNotFound range key))
                            (joinResolutions (HashMap.elems opened) current)
      modify $ over lens (HashMap.insert key (resolved name))
      pure name
  where
    getValue = HashMap.lookup key . (view lens)

    foldResolutions :: [Resolution] -> Resolution -> Resolution
    foldResolutions otherRes mainRes = foldl mergeResolution mainRes otherRes

    resolveRes :: MonadResolve m => m Text -> Maybe Resolution -> m Text
    resolveRes toRet = \case
        Nothing                       -> toRet
        Just (Resolution (res :| [])) -> pure res
        Just (Resolution ambiguity)   -> do
          name <- gets _moduleName
          throwError (AmbiguousNames range name key (Resolution ambiguity))

    joinResolutions :: [Module] -> Module -> Maybe Resolution
    joinResolutions others main =
      let otherRes = catMaybes $ map getValue others in
      foldResolutions otherRes <$> (getValue main)

fixQualify :: MonadResolve m => Range -> Text -> m Text
fixQualify range name = do
  aliases <- asks _aliasedModules
  imports <- asks _importedModules
  case HashMap.lookup name aliases of
    Just res -> pure res
    Nothing  -> maybe (throwError (ModuleNotFound range name)) (const $ pure name) (HashMap.lookup name imports)

getImportedModule :: MonadResolve m => Range -> Text -> m Module
getImportedModule range name = do
  aliases <- asks _aliasedModules
  imports <- asks _importedModules
  let resolvedName = maybe name id (HashMap.lookup name aliases)
  case HashMap.lookup resolvedName imports of
    Just res -> pure res
    Nothing  -> throwError (ModuleNotFound range name)

getBinding :: MonadResolve m => Path Normal -> m (Path Resolved)
getBinding = \case
  (PaExt x              ) -> absurd x
  (Path _    (NaExt x) _) -> absurd x
  (Path ls@(_:_) (Name name' range) _) ->
    resolveCanonicalPath valueDecls range (joinPath ls) range name'
  (Path []       (Name name' range) ext) -> do
    bindings <- asks _localBindings
    curName  <- asks (_moduleName . _currentModule)
    quali <- if HashSet.member name' bindings
                then pure $ curName
                else getOpened valueDecls range name'
    pure $ mkPath ext quali range name'


joinPath :: [Name Normal] -> Text
joinPath = Text.intercalate "." . map getName

resolveCanonicalPath :: MonadResolve m => Lens' Module (HashMap Text Resolution) -> Range -> Text -> Range -> Text -> m (Path Resolved)
resolveCanonicalPath lens modRange oldModName valRange valName = do
  newModName <- fixQualify modRange oldModName
  table <- getImportedModule modRange newModName
  case HashMap.lookup valName (view lens table) of
    Just _   -> pure $ mkPath modRange newModName valRange valName
    Nothing  -> throwError (VariableNotFound valRange (oldModName <> "." <> valName))

mkPath :: Range -> Text -> Range -> Text -> Path Resolved
mkPath modRange modName valRange valName = PaExt (ResPath (Name modName modRange) (Name valName valRange) (modRange <> valRange))

resolvePath :: MonadResolve m => Lens' Module (HashMap Text Resolution) -> Path Normal -> m (Path Resolved)
resolvePath lens = \case
  (PaExt x              ) -> absurd x
  (Path _    (NaExt x) _) -> absurd x
  (Path ls@(_:_) (Name name' range) _) -> resolveCanonicalPath lens range (joinPath ls) range name'
  (Path []       (Name name' range) ext) -> (\x -> mkPath ext x range name') <$> getOpened lens range name'

-- Resolution

initToResolve :: (MonadImport Module m) => StateT (Env, Module) m a -> Module -> m (Env, Module)
initToResolve action mod' = execStateT action (emptyEnv mod', mod')

resolveLit :: Literal Normal -> Literal Resolved
resolveLit = \case
  LStr t x -> LStr t x
  LInt t x -> LInt t x

resolvePattern :: MonadResolve m => Pat Normal -> m (Pat Resolved, HashSet Text)
resolvePattern pat =
    go HashSet.empty pat
  where
    seqGo :: MonadResolve m => HashSet Text -> [Pat Normal] -> m ([Pat Resolved], HashSet Text)
    seqGo bindings []       = pure ([], bindings)
    seqGo bindings (x : xs) = do
      (pat', newBindings) <- go bindings x
      (res, endBindings) <- seqGo newBindings xs
      pure (pat' : res, endBindings)

    go :: MonadResolve m => HashSet Text -> Pat Normal -> m (Pat Resolved, HashSet Text)
    go bindings (PWild ext)       = pure (PWild ext, bindings)
    go _        (PId (NaExt x) _) = absurd x
    go bindings (PId (Name text range) ext)
      | HashSet.member text bindings = throwError (DuplicatedPatId range text)
      | otherwise = pure (PId (Name text range) ext, HashSet.insert text bindings)
    go bindings (PLit lit ext)    = pure (PLit (resolveLit lit) ext, bindings)
    go bindings (PAnn pat' ty ex) = do
      (patRes, bindings') <- go bindings pat'
      tyRes <- resolveType ty
      pure (PAnn patRes tyRes ex, bindings')
    go bindings (PCons p arg x)   = do
      resolvedPath         <- resolvePath consDecls p
      (resArgs, bindings') <- seqGo bindings arg
      pure (PCons resolvedPath resArgs x, bindings')
    go _ (PExt x) = absurd x

resolveBlock :: MonadResolve m => Block Normal -> m (Block Resolved)
resolveBlock = \case
  BlBind expr block         -> BlBind <$> resolveExpr expr <*> resolveBlock block
  BlEnd expr                -> BlEnd  <$> resolveExpr expr
  BlVar (Var pat val ext) block -> do
    (resPat, bindings) <- resolvePattern pat
    resExpr <- resolveExpr val
    resBlock <- withBindings bindings (resolveBlock block)
    pure (BlVar (Var resPat resExpr ext) resBlock)

resolveExpr :: MonadResolve m => Expr Normal -> m (Expr Resolved)
resolveExpr = \case
  Lit lit x -> pure $ Lit (resolveLit lit) x
  Lam pat body x -> uncurry Lam <$> resolveBoth pat body <*> pure x
  App expr args x -> App <$> resolveExpr expr <*> traverse resolveExpr args <*> pure x
  Lower path x -> Lower <$> getBinding path <*> pure x
  Upper path x -> Lower <$> resolvePath consDecls path <*> pure x
  If cond if' els' ext -> If <$> resolveExpr cond <*> resolveExpr if' <*> traverse resolveExpr els' <*> pure ext
  Ann expr ty ext -> Ann <$> resolveExpr expr <*> resolveType ty <*> pure ext
  Accessor expr field ext -> Accessor <$> resolveExpr expr <*> pure (resolveName field)  <*> pure ext
  Case scutinizer fields ext -> Case <$> resolveExpr scutinizer <*> traverse (uncurry resolveBoth) fields <*> pure ext
  Block block ext -> Block <$> resolveBlock block <*> pure ext

resolveBoth :: MonadResolve m => Pat Normal -> Expr Normal -> m (Pat Resolved, Expr Resolved)
resolveBoth pat expr = do
  (resPat, bindings) <- resolvePattern pat
  resExpr <- withBindings bindings (resolveExpr expr)
  pure (resPat, resExpr)

-- TODO: Probably get free polymorphic types and add tforalls for them?
resolveType :: MonadResolve m => Type Normal -> m (Type Resolved)
resolveType = \case
  TId path ext        -> TId <$> resolvePath tyDecls path <*> pure ext
  TPoly name ext      -> pure $ TPoly (resolveName name) ext
  TCons path args ext -> TCons <$> resolvePath tyDecls path <*> traverse resolveType args <*> pure ext
  TArrow from to ext  -> TArrow <$> resolveType from <*> resolveType to <*> pure ext
  TForall name ty ext -> TForall (resolveName name) <$> resolveType ty <*> pure ext

resolveName :: Name Normal -> Name Resolved
resolveName (Name t x) = (Name t x)
resolveName (NaExt v)  = absurd v

withBindings :: MonadResolve m => HashSet Text -> m a -> m a
withBindings bindings action = local (over localBindings (<> bindings)) action

resolveLetDecl :: MonadResolve m => LetDecl Normal -> m (LetDecl Resolved)
resolveLetDecl (LetDecl name args body ret ext) = do
    resArgs     <- traverse resolveNameAndType args
    let bindings = HashSet.fromList (map (getNameResolved . fst) resArgs)
    resBody     <- withBindings bindings (resolveExpr body)
    resRet      <- traverse resolveType ret
    pure (LetDecl (resolveName name) resArgs resBody resRet ext)
  where
    resolveNameAndType :: MonadResolve m => (Name Normal, Type Normal) -> m (Name Resolved, Type Resolved)
    resolveNameAndType (name', ty) = do
      tt <- resolveType ty
      pure (resolveName name', tt)

resolveTypeDecl :: MonadResolve m => TypeDecl Normal -> m (TypeDecl Resolved)
resolveTypeDecl (TypeDecl name args decl) =
    TypeDecl (resolveName name) (map resolveName args) <$> resolveTyDecl decl
  where
    resolveSec :: MonadResolve m => (b -> m c) -> (Name Normal, b) -> m (Name Resolved, c)
    resolveSec action (name', snd') = (\snd'' -> (resolveName name', snd'')) <$> action snd'

    resolveTyDecl :: MonadResolve m => TypeDeclArg Normal -> m (TypeDeclArg Resolved)
    resolveTyDecl = \case
      TypeSym ty      -> TypeSym  <$> resolveType ty
      TypeProd fields -> TypeProd <$> traverse (resolveSec resolveType) fields
      TypeSum fields  -> TypeSum  <$> traverse (resolveSec (traverse resolveType)) fields

resolveProgram :: MonadResolve m => Program Normal -> m (Program Resolved)
resolveProgram (Program tyDefs letDefs _ ext) =
  Program <$> traverse resolveTypeDecl tyDefs
          <*> traverse resolveLetDecl letDefs
          <*> pure []
          <*> pure ext
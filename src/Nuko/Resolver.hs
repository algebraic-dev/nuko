module Nuko.Resolver (
    Env(..),
    Module(..),
    resolveImport,
    resolveProgram,
    mergeRes,
    Resolution(..),
    emptyEnv,
    emptyMod,
) where

import Nuko.Tree.Expr
import Nuko.Tree.TopLevel
import Nuko.Resolver.Resolved
import Nuko.Resolver.Support      (MonadImport (..), ImportResult(..))
import Nuko.Syntax.Ast            (Normal)
import Control.Monad.RWS          (MonadReader (local))
import Control.Monad.Except       (MonadError (throwError))
import Data.HashMap.Strict        (HashMap)
import Data.Text                  (Text)
import Data.HashSet               (HashSet)
import Data.Void                  (absurd)
import Control.Monad              (foldM)
import Control.Monad              (when)
import Control.Monad.Reader       (ask, asks)

import qualified Data.HashSet        as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text           as Text
import Data.Bitraversable (Bitraversable(bitraverse))

data Resolution = Single Text | Ambiguous (HashSet Text)
  deriving Show

data Module = Module
  { moduleName :: Text
  , valueDecls :: HashMap Text Resolution
  , tyDecls    :: HashMap Text Resolution
  , consDecls  :: HashMap Text Resolution
  , fieldDecls :: HashMap Text Resolution
  } deriving Show

data Env = Env
  { localBindings  :: HashSet Text
  , typeBindings   :: HashSet Text
  , loadedModules  :: HashMap Text Module
  , aliasedModules :: HashMap Text Text
  , currentModule  :: Module
  }

type MonadResolve m =
  ( MonadReader Env m
  , MonadImport Module m
  , MonadError Text m
  )

mergeRes :: Resolution -> Resolution -> Resolution
mergeRes (Ambiguous n) (Ambiguous m) = Ambiguous (n <> m)
mergeRes (Ambiguous m) (Single n)    = mergeRes (Single n) (Ambiguous m)
mergeRes (Single n) (Ambiguous m)    = Ambiguous (HashSet.insert n m)
mergeRes (Single name) (Single other)
  | name == other = Single name
  | otherwise     = Ambiguous (HashSet.fromList [name, other])

openModule :: Module -> Module -> Module
openModule aMod bMod = aMod
  { valueDecls = HashMap.unionWith mergeRes aMod.valueDecls bMod.valueDecls
  , tyDecls    = HashMap.unionWith mergeRes aMod.tyDecls bMod.tyDecls
  , consDecls  = HashMap.unionWith mergeRes aMod.consDecls bMod.consDecls
  , fieldDecls = HashMap.unionWith mergeRes aMod.fieldDecls bMod.fieldDecls
  }

modCur :: (Module -> Module) -> Env -> Env
modCur fn env = env { currentModule = fn env.currentModule }

addDef :: Text -> Text -> HashMap Text Resolution -> HashMap Text Resolution
addDef k = HashMap.insertWith mergeRes k . Single

emptyEnv :: Module -> Env
emptyEnv = Env HashSet.empty HashSet.empty HashMap.empty HashMap.empty

emptyMod :: Text -> Module
emptyMod name = Module name HashMap.empty HashMap.empty HashMap.empty HashMap.empty

addVar :: Text -> Env -> Env
addVar text env = env { localBindings = HashSet.insert text env.localBindings }

addVars :: [Text] -> Env -> Env
addVars []       env = env
addVars (x : xs) env = addVars xs (addVar x env)

addModuleToEnv :: Text -> Module -> Env -> Env
addModuleToEnv name mod' env = env { loadedModules = HashMap.insert name mod' env.loadedModules }

addAlias :: Text -> Text -> Env -> Env
addAlias alias original env = env { aliasedModules = HashMap.insert alias original env.aliasedModules }

withVars :: MonadReader Env m => HashSet Text -> m b -> m b
withVars newVars = local (\env -> env { localBindings = env.localBindings <> newVars })

withTys :: MonadReader Env m => HashSet Text -> m b -> m b
withTys newVars = local (\env -> env { typeBindings = env.typeBindings <> newVars })

withTy :: MonadReader Env m => Text -> m b -> m b
withTy newVar = local (\env -> env { typeBindings = HashSet.insert newVar (env.typeBindings) })

getName :: Name Normal -> Text
getName (Name t _) = t
getName (NaExt x) = absurd x

getNameR :: Name Resolved -> Text
getNameR (Name t _) = t
getNameR (NaExt x) = absurd x

getModuleName :: Path Normal -> Text
getModuleName (Path ls@(_ : _) f _) = (Text.intercalate "." (map getName ls)) <> "." <> getName f
getModuleName (Path _ f _)          = getName f
getModuleName (PaExt x)             = absurd x

appendName :: Text -> Text -> Text
appendName "" t = t
appendName x t  = x <> "." <> t

-- Resolvers

-- | So, in the beginning we will just start the entire module just by reading
-- superficial information like the names. Then we just substitute all the paths
-- for paths that we know they exists (we qualify them.)

resolveProgram :: MonadResolve m => Program Normal -> Text -> Module -> m (Program Resolved, Module)
resolveProgram program name prelude = do

  -- Initialization of all the names in the modules.
  mod'   <- foldM initLetDecl (emptyMod name) program.letDecls
  mod''  <- foldM initTypeDecl mod' program.typeDecls
  env    <- foldM resolveImport (emptyEnv mod'') program.impDecl


  let resolutionEnvironment = modCur (`openModule` prelude) env

  res <- local (const resolutionEnvironment) $
    Program
      <$> traverse resolveTyDecl program.typeDecls
      <*> traverse resolveLetDecl program.letDecls

  pure (res [] NoExt, mod'')

-- | This function is useful to import all modules before it runs and resolve them
--   So i'll be able to track a lot of things here. Yes, it receives one module.

resolveImport :: MonadResolve m => Env -> Import Normal -> m Env
resolveImport env (Import path as_ _) = do
  let moduleName = getModuleName path;
  result <- importModule moduleName
  case result of
    NotFound      -> throwError "Not found"
    Succeded mod' -> do
      let env' = addModuleToEnv moduleName mod' env
      case as_ of
        Just alias -> pure $ addAlias (getName alias) moduleName env'
        Nothing    -> pure env'

-- | Initialize all declarations so we can use mutual recursivity and shit like that.

initLetDecl :: MonadResolve m => Module -> LetDecl Normal -> m Module
initLetDecl mod' letDecl =
  pure $ mod' { valueDecls = addDef (getName letDecl.declName) mod'.moduleName mod'.valueDecls }

initTypeDeclArg :: MonadResolve m => Module -> TypeDeclArg Normal -> m Module
initTypeDeclArg mod' = \case
    TypeSym _      -> pure mod'
    TypeProd f     -> pure $ foldr addField mod' f
    TypeSum fields -> pure $ foldr addConst mod' fields
  where
    addField (name, _) mod'' = mod'' { fieldDecls = addDef (getName name) mod'.moduleName mod'.fieldDecls }
    addConst (name, _) mod'' = mod'' { consDecls  = addDef (getName name) mod'.moduleName mod'.consDecls }

initTypeDecl :: MonadResolve m => Module -> TypeDecl Normal -> m Module
initTypeDecl mod' typeDecl = do
  let typeName = getName typeDecl.tyName
  let modName  = appendName mod'.moduleName typeName
  newMod  <- (initTypeDeclArg (emptyMod modName) typeDecl.tyDecl)
  _result <- addModule modName newMod
  pure (mod' { valueDecls = addDef typeName mod'.moduleName mod'.valueDecls })

toRes :: Name Normal -> Name Resolved
toRes (Name e f) = Name e f
toRes (NaExt r) = absurd r

normalizePath :: MonadResolve m => Path Normal -> (Name Normal -> XPath Normal -> m (Path Resolved)) -> m (Path Resolved)
normalizePath (PaExt x)          fn = absurd x
normalizePath (Path [] final r)  fn = fn final r
normalizePath (Path mod' final range) fn = do
    let name = Text.intercalate "." (map getName mod')
    env <- ask
    case (look name env.loadedModules, look name env.aliasedModules) of
      (Just _, _)        -> pure $ PaExt (ResPath (Name name range) (toRes final) range)
      (_, Just alias)    -> pure $ PaExt (ResPath (Name alias range) (toRes final) range)
      (Nothing, Nothing) -> throwError "Cannot find"
  where
    look :: Text -> HashMap Text v -> Maybe v
    look n = HashMap.lookup n

findIn :: MonadResolve m => (Module -> HashMap Text Resolution) ->Name Normal -> XPath Normal -> m (Path Resolved)
findIn getOut name ext = do
  mod' <- asks currentModule
  case HashMap.lookup (getName name) (getOut mod') of
    Just (Single s)    -> pure $ PaExt (ResPath (Name s ext) (toRes name) ext)
    Just (Ambiguous x) -> throwError $ "Ambiguous shit man!" <> Text.pack (show x)
    Nothing            -> throwError $ "Cannot find the fucking " <> (getName name)


nCons :: MonadResolve m => Name Normal -> XPath Normal -> m (Path Resolved)
nCons = findIn consDecls

nDecl :: MonadResolve m => Name Normal -> XPath Normal -> m (Path Resolved)
nDecl name ext = do
  bindings <- asks localBindings
  if HashSet.member (getName name) bindings
    then pure $ PaExt $ ResName (toRes name) ext
    else findIn valueDecls name ext

nTy :: MonadResolve m => Name Normal -> XPath Normal -> m (Path Resolved)
nTy name ext = do
  bindings <- asks typeBindings
  if HashSet.member (getName name) bindings
    then pure $ PaExt $ ResName (toRes name) ext
    else findIn tyDecls name ext

-- | Program resolvers

accPat :: (MonadResolve m) => ([Pat Resolved], HashSet Text) -> Pat Normal -> m ([Pat Resolved], HashSet Text)
accPat (resPaths, bindings') pat = do
  (resPat, bindings'') <- resolvePat' bindings' pat
  pure (resPat : resPaths, bindings'')

resolvePat' :: MonadResolve m => HashSet Text -> Pat Normal -> m (Pat Resolved, HashSet Text)
resolvePat' bindings = \case
    (PWild r)   -> pure (PWild r, bindings)
    (PId id' r) -> do
      when (HashSet.member (getName id') bindings) (throwError "Duplicated")
      pure (PId (toRes id') r, HashSet.insert (getName id') bindings)
    (PCons path pats r) -> do
      newPath <- normalizePath path nCons
      (newPats, bindings') <- foldM accPat ([], bindings) pats
      pure (PCons newPath newPats r, bindings')
    PLit lit x -> pure $ (PLit (resolveLit lit) x, bindings)
    PAnn p t x -> do
      (pat, bindings') <- resolvePat' bindings p
      resTy <- resolveType t
      pure (PAnn pat resTy x, bindings')
    PExt x -> absurd x

resolvePat :: MonadResolve m => Pat Normal -> m (Pat Resolved, HashSet Text)
resolvePat pat = resolvePat' HashSet.empty pat

resolveLit :: Literal Normal -> Literal Resolved
resolveLit (LStr t e) = LStr t e
resolveLit (LInt t e) = LInt t e

resolveBlock :: MonadResolve m => Block Normal -> m (Block Resolved)
resolveBlock = \case
  (BlBind expr rest)         -> BlBind <$> resolveExpr expr <*> resolveBlock rest
  (BlEnd expr)               -> BlEnd <$> resolveExpr expr
  (BlVar (Var pat expr x) x') -> do
    (patRes, bindings) <- resolvePat pat
    var <- Var patRes <$> resolveExpr expr <*> pure x
    (BlVar var) <$> (withVars bindings $ resolveBlock x')

resolveBoth :: MonadResolve m => Pat Normal -> Expr Normal -> m (Pat Resolved, Expr Resolved)
resolveBoth pat expr = do
  (resPat, bindings) <- resolvePat pat
  exprRes <- withVars bindings $ resolveExpr expr
  pure (resPat, exprRes)

resolveType :: MonadResolve m => Type Normal -> m (Type Resolved)
resolveType = \case
  TId path x   -> TId   <$> normalizePath path nTy <*> pure x
  TPoly name x -> pure $ TPoly (toRes name) x
  TCons cons types x -> TCons <$> normalizePath cons nTy <*> traverse resolveType types <*> pure x
  TArrow f t x -> TArrow <$> resolveType f <*> resolveType t <*> pure x
  TForall name ty x -> TForall <$> (pure $ toRes name) <*> withTy (getName name) (resolveType ty) <*> pure x

resolveExpr :: MonadResolve m => Expr Normal -> m (Expr Resolved)
resolveExpr = \case
    Lit a b     -> pure (Lit (resolveLit a) b)
    Lam pat e x -> do
      (patRes, exprRes) <- resolveBoth pat e
      pure (Lam patRes exprRes x)
    App name exprs x -> do
      nameRes  <- resolveExpr name
      exprsRes <- traverse resolveExpr exprs
      pure (App nameRes exprsRes x)
    Lower res x ->
      Lower <$> (normalizePath res nDecl) <*> pure x
    Upper res x ->
       Upper <$> (normalizePath res nDecl) <*> pure x
    Accessor expr field x  ->
      Accessor <$> resolveExpr expr <*> pure (toRes field) <*> pure x
    If cond if' els x ->
      If <$> resolveExpr cond <*> resolveExpr if' <*> (traverse resolveExpr els) <*> pure x
    Case strutinizer pats x -> do
      resStructnizer <- resolveExpr strutinizer
      patsRes <- traverse (uncurry resolveBoth) pats
      pure $ Case resStructnizer patsRes x
    Block block x ->
      Block <$> resolveBlock block <*> pure x
    Ann expr ty x ->
      Ann <$> resolveExpr expr <*> resolveType ty <*> pure x

resolveLetDecl :: MonadResolve m => LetDecl Normal -> m (LetDecl Resolved)
resolveLetDecl (LetDecl name args body ret x) = do
  args'    <- traverse (\(name', ty) -> (toRes name',) <$> resolveType ty) args
  let names = HashSet.fromList (map (getNameR . fst) args')
  ret'     <- traverse resolveType ret
  body'    <- withVars names $ resolveExpr body
  pure (LetDecl (toRes name) args' body' ret' x)

resolveTyDeclArg  :: MonadResolve m => TypeDeclArg Normal -> m (TypeDeclArg Resolved)
resolveTyDeclArg = \case
  TypeSym ty  -> TypeSym  <$> resolveType ty
  TypeProd ty -> TypeProd <$> traverse (bitraverse (pure . toRes) resolveType) ty
  TypeSum ty  -> TypeSum  <$> traverse (bitraverse (pure . toRes) (traverse resolveType)) ty

resolveTyDecl :: MonadResolve m => TypeDecl Normal -> m (TypeDecl Resolved)
resolveTyDecl (TypeDecl name args decl) = do
  declRes <- withTys (HashSet.fromList (map getName args)) (resolveTyDeclArg decl)
  pure (TypeDecl (toRes name) (map toRes args) declRes)
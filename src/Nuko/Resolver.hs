module Nuko.Resolver (
    Module(..),
    resolveImport,
    resolveProgram
) where

import Nuko.Tree.Expr
import Nuko.Tree.TopLevel
import Nuko.Resolver.Resolved
import Nuko.Resolver.Support  (MonadImport (..), ImportResult(..))
import Nuko.Syntax.Ast        (Normal)
import Control.Monad.RWS      (MonadReader (local))
import Control.Monad.Except   (MonadError (throwError))
import Data.HashMap.Strict    (HashMap)
import Data.Text              (Text)
import Data.HashSet           (HashSet)
import Data.Void              (absurd)
import Control.Monad          (foldM)

import qualified Data.HashSet        as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text           as Text
import Control.Monad.Trans.Except (throwE)
import Control.Monad (when)
import Control.Monad.Reader (ask)

data Module = Module
  { childModules :: HashMap Text Module
  , valueDecls   :: HashSet Text
  , tyDecls      :: HashSet Text
  , consDecls    :: HashSet Text
  }

data Env = Env
  { localBindings  :: HashSet Text
  , loadedModules  :: HashMap Text Module
  , aliasedModules :: HashMap Text Text
  , currentModule  :: Module }

type MonadResolve m =
  ( MonadReader Env m
  , MonadImport Module m
  , MonadError Text m
  )

emptyEnv :: Module -> Env
emptyEnv = Env HashSet.empty HashSet.empty HashMap.empty HashMap.empty

emptyMod :: Module
emptyMod = Module HashMap.empty HashSet.empty HashSet.empty

addVar :: Text -> Env -> Env
addVar text env = env { localBindings = HashSet.insert text env.localBindings }

addVars :: [Text] -> Env -> Env
addVars []       env = env
addVars (x : xs) env = addVars xs (addVar x env)

addModule :: Text -> Module -> Env -> Env
addModule name mod' env = env { loadedModules = HashMap.insert name mod' env.loadedModules }

addAlias :: Text -> Text -> Env -> Env
addAlias alias original env = env { aliasedModules = HashMap.insert alias original env.aliasedModules }

withVars :: MonadReader Env m => [Text] -> m b -> m b
withVars text = local (addVars text)

getName :: Name Normal -> Text
getName (Name t _) = t
getName (NaExt x) = absurd x

getModuleName :: Path Normal -> Text
getModuleName (Path ls@(_ : _) f _) = (Text.intercalate "." (map getName ls)) <> "." <> getName f
getModuleName (Path _ f _)          = getName f
getModuleName (PaExt x)             = absurd x

-- Resolvers

-- | So, in the beginning we will just start the entire module just by reading
-- superficial information like the names. Then we just substitute all the paths
-- for paths that we know they exists (we qualify them.)
resolveProgram :: MonadResolve m => Program Normal -> m (Program Resolved, Module)
resolveProgram program = do

  mod'   <- foldM initLetDecl emptyMod program.letDecls
  mod''  <- foldM initTypeDecl mod' program.tyDecls
  env    <- foldM resolveImport (emptyEnv mod'') program.impDecl

  undefined

-- | This function is useful to import all modules before it runs and resolve them
--   So i'll be able to track a lot of things here. Yes, it receives one module.

resolveImport :: MonadResolve m => Env -> Import Normal -> m Env
resolveImport env (Import path as_ _) = do
  let moduleName = getModuleName path;
  result <- importModule moduleName
  case result of
    Succeded mod' -> do
      let env' = addModule moduleName mod' env
      case as_ of
        Just alias -> pure $ addAlias (getName alias) moduleName env'
        Nothing    -> pure env'
    NotFound      -> throwError "Not found"

-- | Initialize all declarations so we can use mutual recursivity and shit like that.

initLetDecl :: MonadResolve m => Module -> LetDecl Normal -> m Module
initLetDecl mod' letDecl =
  pure $ mod' { valueDecls = HashSet.insert (getName letDecl.declName) mod'.valueDecls }

initTypeDecl :: MonadResolve m => Module -> TypeDecl Normal -> m Module
initTypeDecl mod' typeDecl =
  pure $ mod' { valueDecls = HashSet.insert (getName typeDecl.tyName) mod'.valueDecls }

toRes :: Name Normal -> Name Resolved
toRes (Name e f) = Name e f
toRes (NaExt r) = absurd r

normalizePath :: MonadResolve m => Path Normal -> m (Path Resolved)
normalizePath (PaExt x)          = absurd x
normalizePath (Path [] final r)  = pure $ PaExt (ResName (toRes final) r)
normalizePath (Path mod' final range) = do
    let name = Text.intercalate "." (map getName mod')
    env <- ask
    case (look name env.loadedModules, look name env.aliasedModules) of
      (Just _, _)        -> pure $ PaExt (ResPath (Name name range) (toRes final) range)
      (_, Just alias)    -> pure $ PaExt (ResPath (Name alias range) (toRes final) range)
      (Nothing, Nothing) -> throwError "Cannot find"
  where
    look :: k -> HashMap k v -> Maybe v
    look n = HashMap.lookup n

-- | Program resolvers

resolvePat' :: MonadResolve m => HashSet Text -> Pat Normal -> m (Pat Resolved, HashSet Text)
resolvePat' bindings = \case
    (PWild r)   -> pure (PWild r, bindings)
    (PId id' r) -> do
      when (HashSet.member (getName id') bindings) (throwError "Duplicated")
      pure (PId (toRes id') r, HashSet.insert (getName id') bindings)
    (PCons path pats r) -> do
      newPath <- normalizePath path
      (newPats, bindings') <- foldM accPat ([], bindings) pats
      pure (PCons newPath newPats r, bindings')
    _ -> undefined
  where
    accPat :: ([Pat Resolved], HashSet Text) -> Pat Normal -> m ([Pat Resolved], HashSet Text)
    accPat (resPaths, bindings') pat = do
      (resPat, bindings'') <- resolvePat' bindings' pat
      pure (resPat : resPaths, bindings'')

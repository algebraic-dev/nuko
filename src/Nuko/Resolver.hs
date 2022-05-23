module Nuko.Resolver (
    Module(..),
    resolveImport,
    resolveProgram
) where

import Data.HashMap.Strict    (HashMap)
import Data.Text              (Text)

import Nuko.Tree.TopLevel
import Nuko.Resolver.Resolved (Resolved)
import Nuko.Resolver.Support  (MonadImport)
import Nuko.Syntax.Ast        (Normal)

data Module = Module
    { childModules :: HashMap Text Module
    , valueDecls   :: HashMap Text Text
    , tyDecls      :: HashMap Text Text
    }

resolveProgram :: MonadImport Module m => Program Normal -> m (Program Resolved, Module)
resolveProgram program = do
    imports <- traverse resolveImport program.impDecl
    undefined

resolveImport :: MonadImport Module m => Import Normal -> m (Import Resolved, Module)
resolveImport (Import path as_ range) = undefined
module Typer.Context where

import Typer.Types

import Data.Map (Map)
import Data.Text (Text)

import qualified Data.Map    as Map
import Syntax.Bounds (Bounds)

data Ctx
  = Ctx { ctxTypeLvl :: Lvl
        , ctxKindLvl :: Lvl
        , ctxVars    :: Map Text (Type 'Eval)
        , ctxHoles   :: Map Text (Kind 'Eval)
        , ctxTypes   :: Map Text KindScheme
        , ctxPos     :: Bounds
        }

upKindLvl :: Ctx -> Ctx
upKindLvl ctx = ctx { ctxKindLvl = ctxKindLvl ctx + 1 }

addKind :: Ctx -> Text -> KindScheme -> Ctx
addKind ctx k v = ctx { ctxTypes = Map.insert k v (ctxTypes ctx) }

addTy :: Ctx -> Text -> Type 'Eval -> Ctx
addTy ctx k v = ctx { ctxVars = Map.insert k v (ctxVars ctx) }

addToEval :: Ctx -> Text -> Kind 'Eval -> Ctx
addToEval ctx k v = ctx { ctxHoles = Map.insert k v (ctxHoles ctx) }

setPos :: Ctx -> Bounds -> Ctx
setPos ctx pos = ctx { ctxPos = pos }


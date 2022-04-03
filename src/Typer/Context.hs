module Typer.Context (
  Ctx(..),
  upKindLvl,
  addKind,
  addTy,
  addToEval,
  setPos
) where

import Typer.Types
import Data.Map      (Map)
import Data.Text     (Text)
import Syntax.Range (Range, Ranged (Ranged))

import qualified Data.Map as Map

data Ctx
  = Ctx { ctxTypeLvl :: Lvl
        , ctxKindLvl :: Lvl
        , ctxVars    :: Map Text (TType 'Eval)
        , ctxHoles   :: Map Text (Kind 'Eval)
        , ctxTypes   :: Map Text KindScheme
        , ctxPos     :: Range
        }

upKindLvl :: Ctx -> Ctx
upKindLvl ctx = ctx { ctxKindLvl = ctxKindLvl ctx + 1 }

addKind :: Ctx -> Text -> KindScheme -> Ctx
addKind ctx k v = ctx { ctxTypes = Map.insert k v (ctxTypes ctx) }

addTy :: Ctx -> Text -> TType 'Eval -> Ctx
addTy ctx k v = ctx { ctxVars = Map.insert k v (ctxVars ctx) }

addToEval :: Ctx -> Range -> Text -> Kind 'Eval -> Ctx
addToEval ctx _ k v = ctx { ctxHoles = Map.insert k v (ctxHoles ctx) }

setPos :: Ctx -> Range -> Ctx
setPos ctx pos = ctx { ctxPos = pos }


module Type.Context where

import Syntax.Parser.Ast (Normal)
import Type.Types (TypeKind(..), Type(..))
import Data.Sequence (Seq)

import qualified Type.Types    as Types
import qualified Syntax.Expr   as Expr
import qualified Data.Sequence as Sequence

data CtxElem
  = CtxTy Types.TypeId
  | CtxExists Types.TypeId
  | CtxMarker Types.TypeId
  | CtxSolved Types.TypeId (Types.Type 'Mono)
  | CtxVar (Expr.Name Normal) (Types.Type 'Poly)
  deriving Eq

newtype Ctx = Ctx (Seq CtxElem) deriving (Semigroup, Monoid)

-- Context operations

-- These is..Elem are necessary to avoid some big changes in the future
-- while i try to annotate the elems with position and shit like that.

(|>) :: Ctx -> CtxElem -> Ctx 
(|>) (Ctx ctx) elm = Ctx (ctx Sequence.|> elm)

isElem :: (CtxElem -> Bool) -> Ctx -> Bool 
isElem pred' (Ctx ctx) = not . null $ Sequence.filter pred' ctx

hasTy :: Types.TypeId -> Ctx -> Bool
hasTy elm = isElem $ \case {CtxTy name -> name == elm; _ -> False}

isSolved :: Types.TypeId -> Ctx -> Bool
isSolved elm = isElem $ \case {CtxSolved var _ -> var == elm; _ -> False}

hasExist :: Types.TypeId -> Ctx -> Bool
hasExist name = isElem isExt
  where isExt (CtxExists x)   = x == name 
        isExt (CtxSolved x _) = x == name 
        isExt _ = False

-- Type operations with contexts

typeWellFormed :: Ctx -> Type a -> Bool
typeWellFormed ctx = \case
  TyAlpha _ txt      | txt `hasTy` ctx -> True
  TyExists _ txt     | txt `hasExist` ctx -> True
  TyForall _ txt ty -> typeWellFormed (ctx |> CtxTy txt) ty
  TyFun _ ty ty'    -> typeWellFormed ctx ty && typeWellFormed ctx ty' 
  _                 -> False

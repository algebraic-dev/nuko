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
  deriving (Eq, Show)

newtype Ctx = Ctx { unwrapCtx :: Seq CtxElem }
  deriving (Semigroup, Monoid)

deriving instance (Show Ctx)

-- Context operations

-- These is..Elem are necessary to avoid some big changes in the future
-- while i try to annotate the elems with position and shit like that.

(|>) :: Ctx -> CtxElem -> Ctx
(|>) (Ctx ctx) elm = Ctx (ctx Sequence.|> elm)

isElem :: (CtxElem -> Bool) -> Ctx -> Bool
isElem pred' (Ctx ctx) = not . null $ Sequence.filter pred' ctx

hasTy :: Types.TypeId -> Ctx -> Bool
hasTy elm = isElem $ \case {CtxTy name -> name == elm; _ -> False}

hasMarker :: Types.TypeId -> Ctx -> Bool
hasMarker elm = isElem $ \case {CtxMarker name -> name == elm; _ -> False}

hasVar :: Expr.Name Normal -> Ctx -> Bool
hasVar elm = isElem $ \case {CtxVar name _ -> name == elm; _ -> False}

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
  TyAlpha _ txt     -> txt `hasTy` ctx -- UvarWF
  TyExists _ txt    -> txt `hasExist` ctx -- EvarWF / SolvedEvarWF
  TyForall _ txt ty -> typeWellFormed (ctx |> CtxTy txt) ty -- ForallWF2
  TyFun _ ty ty'    -> typeWellFormed ctx ty && typeWellFormed ctx ty' -- ArrowWF

ctxWellFormed :: Ctx -> Bool
ctxWellFormed (Ctx Sequence.Empty) = True -- EmptyCtx
ctxWellFormed (Ctx (xs Sequence.:|> x)) =
      go (Ctx xs) x && ctxWellFormed (Ctx xs)
    where go ctx = \case
            CtxTy txt        -> not $ txt `hasTy` ctx -- UvarCtx
            CtxExists txt    -> not $ txt `hasExist` ctx -- EvarCtx
            CtxMarker txt    -> not (txt `hasExist` ctx || txt `hasMarker` ctx) --SolvedEvarCtx
            CtxSolved txt ty -> not (txt `hasExist` ctx) && typeWellFormed ctx ty --SolvedEvarCtx
            CtxVar name ty   -> not (name `hasVar` ctx) && typeWellFormed ctx ty -- VarCtx

-- Apply context element 

applyCtxElem :: CtxElem -> Type 'Poly -> Type 'Poly 
applyCtxElem ctxEl ty = case ctxEl of 
  CtxSolved txt ty' -> Types.typeSubst txt (Types.toPoly ty') ty 
  _ -> case ty of 
    TyForall m_bo txt ty' -> TyForall m_bo txt (applyCtxElem ctxEl ty')
    TyFun m_bo ty' ty2 -> TyFun m_bo (applyCtxElem ctxEl ty') (applyCtxElem ctxEl ty2)
    other -> other

applyCtx :: Ctx -> Type 'Poly -> Type 'Poly 
applyCtx (Ctx Sequence.Empty) ty = ty
applyCtx (Ctx (xs Sequence.:|> x)) ty = applyCtx (Ctx xs) (applyCtxElem x ty) 

-- replace element 

replaceCtx :: Ctx -> CtxElem -> [CtxElem] -> Ctx 
replaceCtx (Ctx ctx) el els = 
  let (r, l) = Sequence.spanr (/= el) ctx in 
  case l of 
    Sequence.Empty -> Ctx (Sequence.fromList els Sequence.>< r)
    (xs Sequence.:|> _) -> Ctx (xs Sequence.>< Sequence.fromList els Sequence.>< r)

seqRem :: Seq a -> Seq a 
seqRem Sequence.Empty      = Sequence.Empty
seqRem (xs Sequence.:|> _) = xs 

dropUntil :: Ctx -> CtxElem -> Ctx 
dropUntil (Ctx ctx) elm = Ctx $ seqRem $ Sequence.dropWhileR (/= elm) ctx
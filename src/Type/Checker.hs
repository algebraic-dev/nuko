module Type.Checker where

import Type.State (Typer)
import Type.Types (Type(..), TypeKind(Mono, Poly))
import Type.Context (CtxElem(..))

import qualified Syntax.Expr          as Expr
import qualified Type.Types           as Types
import qualified Type.Context         as Context
import qualified Type.State           as State
import qualified Control.Monad.Except as Err
import qualified Data.Set as Set
import Syntax.Parser.Ast (Normal)

subType :: Typer m => Type 'Poly -> Type 'Poly -> m ()
subType ty ty' = do
  State.isWellFormed ty
  State.isWellFormed ty'
  ctx <- State.getCtx
  case (ty, ty') of
    (TyAlpha _ a, TyAlpha _ b)   | a == b -> pure ()
    (TyExists _ a, TyExists _ b) | a == b -> pure ()
    (TyFun _ a1 a2, TyFun _ b1 b2) -> do
      subType b1 a1
      a2' <- State.applyCtx a2
      b2' <- State.applyCtx b2
      subType a2' b2'
    (a, TyForall _ alpha fb) -> do
      newA <- State.freshName
      State.mark (CtxTy newA)
                 (subType a (Types.typeSubst alpha (TyAlpha Nothing newA) fb))
    (TyForall _ alpha fa, b) -> do
      newA <- State.freshName
      State.markList (CtxMarker newA) [CtxExists newA]
                     (subType (Types.typeSubst alpha (TyExists Nothing newA) fa) b)
    (TyExists _ a, fa) | a `Context.hasExist` ctx && not (a `Set.notMember` Types.tyFreeVars fa) -> instantiateL a fa
    (fa, TyExists _ b) | b `Context.hasExist` ctx && not (b `Set.notMember` Types.tyFreeVars fa) -> instantiateR fa b
    (_, _) -> Err.throwError "Subtype error"

instantiateL :: Typer m => Types.TypeId -> Type 'Poly -> m ()
instantiateL id' ty = do
  case Types.toMono ty of 
    Just ty' -> State.replace (CtxExists id') [CtxSolved id' ty']
    Nothing -> case ty of -- TEXists needs ordered
      TyExists m_bo txt -> State.replace (CtxExists txt) [CtxSolved txt (TyExists m_bo id')]
      TyForall m_bo txt ty' -> do 
          b <- State.freshName 
          State.mark (CtxTy b) (instantiateL id' (Types.typeSubst txt (TyAlpha m_bo b) ty'))
      TyFun m_bo ty1 ty2 -> do 
          a <- State.freshName 
          b <- State.freshName
          State.replace (CtxExists id') 
            [ CtxExists a
            , CtxExists b
            , CtxSolved id' (TyFun m_bo (TyExists Nothing a) (TyExists Nothing b))]
          instantiateR ty1 a
          instantiateL b =<< State.applyCtx ty2
      _ -> error "Impossible."

  undefined

instantiateR :: Typer m => Type 'Poly -> Types.TypeId -> m ()
instantiateR ty id' = do
  case Types.toMono ty of 
    Just ty' -> State.replace (CtxExists id') [CtxSolved id' ty']
    Nothing -> case ty of -- TEXists needs ordered
      TyExists m_bo txt -> State.replace (CtxExists txt) [CtxSolved txt (TyExists m_bo id')]
      TyForall m_bo txt ty' -> do 
          b <- State.freshName 
          State.markList (CtxMarker b) [CtxExists b] (instantiateL id' (Types.typeSubst txt (TyExists m_bo b) ty'))
      TyFun m_bo ty1 ty2 -> do 
          a <- State.freshName 
          b <- State.freshName
          State.replace (CtxExists id')  [ CtxExists a, 
                                           CtxExists b, 
                                           CtxSolved id' (TyFun m_bo (TyExists Nothing a) (TyExists Nothing b))]
          instantiateL a ty1
          res <- State.applyCtx ty2
          instantiateR res b
      _ -> error "Impossible."

typeSynth :: Typer m => Expr.Expr Normal -> m (Types.Type 'Poly)
typeSynth = undefined 

typeCheck :: Typer m => Expr.Expr Normal -> Types.Type 'Poly -> m ()
typeCheck = undefined

typeApply :: Typer m => Types.Type 'Poly -> Expr.Expr Normal -> m (Types.Type 'Poly)
typeApply = undefined
module Typer.Unify where 

import Typer.Types

import qualified Typer.Context as Ctx
import qualified Control.Monad.Except as ER

unifyPreChecks :: Ctx.TypeState m => Lvl -> Lvl -> Ty -> m ()
unifyPreChecks  hole scope = go
    where
      go = \case
        TyCon _          -> pure ()
        TyFun ty' ty''   -> go ty' >> go ty''
        TyForall _ ty'   -> go ty'
        TyBound n        ->
          if n >= scope
            then ER.throwError $ "Type variable '" ++ show n ++ "' is escaping it's scope"
            else pure ()
        TyExists n       -> do 
          hole' <- Ctx.getHole n 
          if n == hole 
            then ER.throwError "Infinite type lol"
            else case hole' of
                  Empty i -> if i > scope 
                      then Ctx.setHole n (Empty scope)
                      else pure ()
                  Filled ty -> go ty
                  
unifyHole :: Ctx.TypeState m => Ctx.Ctx -> Lvl -> Ty -> m ()
unifyHole ctx lvl ty = do
    hole <- Ctx.getHole lvl
    case hole of
      Empty scope ->
          if ty == TyExists lvl
            then pure ()
            else do unifyPreChecks lvl scope ty
                    Ctx.setHole lvl (Filled ty)
      Filled ty' -> unify ctx ty' ty


unify :: Ctx.TypeState m => Ctx.Ctx -> Ty -> Ty -> m ()
unify ctx ty ty' = case (ty, ty') of
        (TyExists a, ty'') -> unifyHole ctx a ty''
        (ty'', TyExists a) -> unifyHole ctx a ty''
        (TyCon a, TyCon b) | a == b -> pure ()
                           | otherwise -> mismatch
        (TyFun a b, TyFun a' b') -> unify ctx a a' >> unify ctx b b'
        (TyBound a, TyBound b) | a == b -> pure ()
                               | otherwise -> mismatch
        (TyForall name tyA, TyForall name' tyB) -> do
            let nCtx = Ctx.addTy ctx
            tyA' <- Ctx.subst name (TyBound (Ctx.lvl ctx)) tyA
            tyB' <- Ctx.subst name' (TyBound (Ctx.lvl ctx)) tyB
            unify nCtx tyA' tyB'
        _ -> mismatch
    where
        mismatch :: Ctx.TypeState m => m a
        mismatch = ER.throwError $ "Mismatch between '" ++ show ty ++ "' and '" ++ show ty' ++ "'"

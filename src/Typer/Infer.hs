module Typer.Infer where

import Syntax.Parser.Ast (Normal)
import Syntax.Expr
import Data.Functor (($>))
import Control.Monad (foldM)
import Typer.Types
import Typer.Unify 

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Typer.Context as Ctx
import qualified Control.Monad.Except as ER
import Data.Set (Set)

bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f ma mb = ma >>= (\a -> mb >>= \b -> f a b)

findName :: Ctx.TypeState m => Ctx.Ctx -> Name Normal -> m Ty
findName ctx (Name _ id') =
    maybe (ER.throwError $ "Cannot find variable '" ++ show id' ++ "'")
          pure
          (Map.lookup id' (Ctx.ctxEnv ctx))

inferLit :: Ctx.TypeState m => Literal Normal -> m Ty
inferLit = \case
  LChar {}   -> pure (TyCon (Kinded "Char" Star))
  LString {} -> pure (TyCon (Kinded "String" Star))
  LInt {}    -> pure (TyCon (Kinded "Int" Star))
  LDouble {} -> pure (TyCon (Kinded "Double" Star))

-- Patterns

checkPat :: Ctx.TypeState m => Ctx.Ctx -> Pattern Normal -> Ty -> m ()
checkPat ctx pat ty = case (pat, ty) of
    (_, TyExists hole) -> do
        hole' <- Ctx.getHole hole
        case hole' of
          Empty _ -> do -- Someday i'll fix it lol
            inferredTy <- inferPat ctx pat
            unify ctx inferredTy ty
          Filled ty' -> checkPat ctx pat ty'
    _ -> do
        inferredTy <- inferPat ctx pat
        unify ctx inferredTy ty

applyPat :: Ctx.TypeState m => Ctx.Ctx ->  Ty -> Pattern Normal -> m Ty
applyPat ctx t pat = case t of
  TyFun ty ty'    -> checkPat ctx pat ty $> ty'
  TyForall txt ty -> do
    instTy <- Ctx.instantiate ctx txt ty
    applyPat ctx instTy pat
  TyExists n  -> do
    hole <- Ctx.getHole n
    case hole of
      Empty i -> do
          h1 <- TyExists <$> Ctx.createHole i
          h2 <- TyExists <$> Ctx.createHole i
          Ctx.setHole n (Filled (TyFun h1 h2))
          checkPat ctx pat h1
          pure h2
      Filled ty -> applyPat ctx ty pat

  other          -> ER.throwError $ "'" ++ show other ++ "' Is not a constructor type"

inferPat :: Ctx.TypeState m => Ctx.Ctx -> Pattern Normal -> m Ty
inferPat ctx = \case
    PWild _           -> fmap TyExists (Ctx.createHole (Ctx.lvl ctx)) -- Creates a hole for this thing that can be anything.
    PId _ (Name _ _)  -> fmap TyExists (Ctx.createHole (Ctx.lvl ctx))
    PLit _ lit        -> inferLit lit
    PCons _ name pats -> do
      cons <- findName ctx name
      let len = countArgs cons
      if len /= length pats 
        then ER.throwError $ "The constructor '" ++ show name ++ "' should have " ++ show len ++ " but got " ++ show (length pats)
        else pure () 
      foldM (applyPat ctx) cons pats
  where 
    countArgs :: Ty -> Int 
    countArgs (TyFun _ b@TyFun {}) = 1 + countArgs b
    countArgs TyFun {} = 1 
    countArgs _ = 0
    
astToTy :: Ctx.TypeState m => Ctx.Ctx -> Typer Normal -> m Ty
astToTy ctx ty = do 
    (fv, ty') <- go Set.empty ty
    pure $ foldr TyForall ty' fv
  where 
    go set = \case
      TSimple _ (Name _ na)      -> do 
        kind' <- Ctx.findKind ctx na
        pure (set, TyCon (Kinded na kind'))
      TPoly _ (Name _ na) -> 
        pure (Set.insert na set, TyCon (Kinded na Star)) -- Probably it will make me have problems in the future with things like (f a -> a)
      TArrow   _ ty' ty'' -> do  
        (s1, t)  <- go set ty'
        (s2, t') <- go s1 ty''
        pure (s2, TyFun t t')
      TCons   _ (Name _ cons) types -> do 
        kind'        <- Ctx.findKind ctx cons
        (fv, rTypes) <- foldM (\(s, t) m -> do 
                          (s2, t2) <- go s m
                          pure (s2, t2 : t)) (set, []) types
        pure (fv, foldl TyApp (TyCon (Kinded cons kind')) (reverse rTypes))
      TForall _ (Name _ name) ty' -> do 
        (fv, t) <- go set ty'
        pure (fv, TyForall name t)

inferBinder :: Ctx.TypeState m => Ctx.Ctx -> Binder Normal -> m Ty
inferBinder ctx = \case  
  Typed _ pat ty -> do
    resTy <- astToTy ctx ty 
    checkPat ctx pat resTy 
    pure resTy
  Raw   _ pat -> inferPat ctx pat
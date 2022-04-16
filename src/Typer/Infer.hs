module Typer.Infer where

import Typer.Env
import Expr
import Syntax.Range (Loc(Loc), getPos)
import Syntax.Tree (Normal)
import Data.Function (on)
import Data.IORef (readIORef)
import Control.Monad.IO.Class (liftIO)
import Typer.Error (TypeError(CannotUnify))
import Control.Exception (throwIO)

unify :: TyperMonad m => Ty -> Ty -> m ()
unify ty ty' = track ((TrUnify `on` getTyPos) ty ty') $ case (ty, ty') of 
    (TyHole _ hole, b) -> do
        resHole <- liftIO $ readIORef hole 
        case resHole of 
            Empty _ _ -> instantiateLeft hole b
            Filled t -> unify t b
    (a, TyHole _ hole) -> do 
        resHole <- liftIO $ readIORef hole 
        case resHole of 
            Empty _ _ -> instantiateRight a hole
            Filled t -> unify a t
    (a, TyForall _ binder ty'') -> 
        scopeUp $ scopeTy binder $ unify a ty''
    (TyForall _ _ _, a) -> do 
        instTy <- instantiate ty
        unify instTy a
    (TyFun _ a b, TyFun _ a' b') -> unify a a' >> unify b b'
    (TyNamed _ a, TyNamed _ a')
        | a == a' -> pure ()
        | otherwise -> liftIO $ throwIO (CannotUnify ty ty')
    (TyRigid _ _ a, TyRigid _ _ a') 
        | a == a' -> pure ()
        | otherwise -> liftIO $ throwIO (CannotUnify ty ty')
    _ -> liftIO $ throwIO (CannotUnify ty ty')

instantiateLeft :: TyperMonad m => TyHole -> Ty -> m ()
instantiateLeft = undefined 

instantiateRight :: TyperMonad m => Ty -> TyHole -> m ()
instantiateRight = undefined 


inferLit :: TyperMonad m => Literal Normal -> m Ty
inferLit lit = track (TrInfer (getPos lit)) $ case lit of 
    LString loc _ -> pure (TyNamed (Loc loc) "String") 
    LChar loc _   -> pure (TyNamed (Loc loc) "Char") 
    LInt loc _    -> pure (TyNamed (Loc loc) "Int") 
    LDouble loc _ -> pure (TyNamed (Loc loc) "Duble") 

inferTy :: TyperMonad m => Typer Normal -> m Ty 
inferTy ty = track (TrInfer (getPos ty)) $ case ty of 
    -- Add existing checking
    TSimple _ name      -> pure (TyNamed (Loc name.loc) name.ident)
    TPoly _ name        -> pure (TyNamed (Loc name.loc) name.ident)
    TArrow loc ty' ty'' -> TyFun (Loc loc) <$> inferTy ty' <*> inferTy ty''
    _ -> error "Not implemented yet"

checkExpr :: TyperMonad m => Expr Normal -> Ty -> m ()
checkExpr expr ty = track (TrCheck (getPos expr) (getTyPos ty)) $ case (expr, ty) of 
    (_, TyHole loc hole) -> undefined
    (_, TyForall loc binder name) -> undefined 
    (Lam loc var body, TyFun loc' left right) -> undefined 
    (Lam loc var body, other) -> undefined 
    (expr', ty') -> do 
        inferred <- inferExpr expr' 
        unify inferred ty' 

inferExpr :: TyperMonad m => Expr Normal -> m Ty 
inferExpr expr = track (TrInfer (getPos expr)) $ case expr of 
    Lam loc (Raw _ (PId _ (Name _ text))) body -> undefined
    App loc left right -> undefined 
    Var loc (Name _ text) -> undefined
    Lit _ lit -> inferLit lit 
    Ann _ expr' ty -> do 
        iTy <- inferTy ty
        checkExpr expr' iTy
        pure iTy
    _ -> error "Not implemented yet"
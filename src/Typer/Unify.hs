module Typer.Unify where 

import Typer.Env
import Data.Function (on)
import Data.IORef (readIORef, writeIORef)
import Control.Monad.IO.Class (liftIO)
import Typer.Error (TypeError(CannotUnify, EscapingScope, OccoursCheck))
import Control.Exception (throwIO)
import Control.Monad (when)

-- Occours and scope checking :D
unifyPreCheck :: TyperMonad m => Lvl -> TyHole -> Ty -> m ()
unifyPreCheck scope hole ty = 
        preCheck ty 
    where 
        preCheck :: TyperMonad m => Ty -> m ()
        preCheck ty' = case ty of
            TyNamed _ _     -> pure () 
            TyFun _ l r     -> preCheck l >> preCheck r 
            TyForall _ _ t  -> preCheck t
            TyRigid _ _ lvl  | lvl > scope -> liftIO $ throwIO $ (EscapingScope ty') 
                             | otherwise  -> pure ()
            TyHole loc hol' -> do
                when (hol' == hole) 
                     (liftIO $ throwIO $ (OccoursCheck (TyHole loc hole) ty))
                resHol <- liftIO $ readIORef hol'
                case resHol of 
                    Empty loc' scope' -> 
                        when (scope' > scope) 
                             (liftIO $ writeIORef hol' (Empty loc' scope)) 
                    Filled t -> preCheck t

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

unifyHole :: TyperMonad m => TyHole -> Ty -> m ()
unifyHole hole ty = do
    resHole <- liftIO $ readIORef hole 
    case resHole of 
        Filled t -> unify t ty
        Empty _ scope -> do 
            case ty of 
                TyHole loc _ | ty == (TyHole loc hole) -> pure () 
                _ -> do 
                    unifyPreCheck scope hole ty
                    liftIO $ writeIORef hole (Filled ty)

instantiateLeft :: TyperMonad m => TyHole -> Ty -> m ()
instantiateLeft hole = \case 
    TyForall _ binder ty -> scopeTy binder $ instantiateLeft hole ty 
    TyFun loc arg ret -> do
        argHole <- newHole 
        retHole <- newHole
        let funType = TyFun loc (TyHole (getTyPos arg) argHole) (TyHole (getTyPos ret) retHole) 
        liftIO $ writeIORef hole $ Filled $ funType
        instantiateRight arg argHole 
        instantiateLeft retHole ret
    ty -> unifyHole hole ty
                
instantiateRight :: TyperMonad m => Ty -> TyHole -> m ()
instantiateRight ty hole = case ty of 
    ty'@(TyForall {}) -> do 
        instTy <- instantiate ty'
        instantiateRight instTy hole
    TyFun loc arg ret -> do
        argHole <- newHole 
        retHole <- newHole
        let funType = TyFun loc (TyHole (getTyPos arg) argHole) (TyHole (getTyPos ret) retHole) 
        liftIO $ writeIORef hole $ Filled $ funType
        instantiateLeft argHole arg
        instantiateRight ret retHole
    ty' -> unifyHole hole ty'
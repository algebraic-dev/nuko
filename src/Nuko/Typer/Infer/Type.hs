module Nuko.Typer.Infer.Type (
    inferTy
) where

import Nuko.Tree.Expr    hiding (Name)
import Nuko.Typer.Error  (typeError, ErrCause(CannotFindType))
import Nuko.Typer.Env    (track, Tracker(..), TyperMonad, Name, localTypes)
import Nuko.Typer.Types  (Ty(..))
import Nuko.Syntax.Ast   (Normal)
import Data.Set          (Set)
import Nuko.Syntax.Range (HasPosition(getPos), Range)

import qualified Data.Set            as Set
import qualified Data.HashMap.Strict as HashMap
import qualified Control.Monad.RWS   as State

getTy :: TyperMonad m => Range -> Name -> m Ty
getTy range name = do
  result <- State.gets (HashMap.lookup name . localTypes)
  maybe
    (typeError (CannotFindType range name))
    (pure . TyRef range)
    result

inferTy :: TyperMonad m => Type Normal -> m Ty
inferTy t = track (InInferTy t) $ do
    (ty, unbounded) <- go t
    pure (foldl (flip $ TyForall (getPos t)) ty unbounded)
  where
    go :: TyperMonad m => Type Normal -> m (Ty, Set Name)
    go ty = track (InInferTy ty) $ case ty of
      TId name _       -> do
        ty' <- getTy name.final.loc name.final.ident
        pure (ty', Set.empty)
      TPoly name _         ->
        pure (TyNamed name.loc name.ident, Set.singleton name.ident)
      TArrow ty' ty'' loc' -> do
        (rTy, setA) <- go ty'
        (rTy', setB) <- go ty''
        pure (TyFun loc' rTy rTy', Set.union setA setB)
      TForall name ty' loc' -> do
        (rTy', set) <- go ty'
        pure (TyForall loc' name.ident rTy', Set.delete name.ident set)
      _  -> error "Not implemented yet"

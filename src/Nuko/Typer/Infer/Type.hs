module Nuko.Typer.Infer.Type (
    inferTy
) where

import Nuko.Tree.Expr    hiding (Name)
import Nuko.Typer.Error  (typeError, ErrCause(CannotFindType))
import Nuko.Typer.Env    (track, Tracker(..), MonadTyper, Name, localTypes)
import Nuko.Typer.Types  (Ty(..))
import Nuko.Syntax.Ast   (Normal)
import Data.Set          (Set)
import Nuko.Syntax.Range (HasPosition(getPos), Range)

import qualified Data.Set            as Set
import qualified Data.HashMap.Strict as HashMap
import qualified Control.Monad.RWS   as State
import qualified Nuko.Tree.Expr as Expr

getTy :: MonadTyper m => Range -> Name -> m Ty
getTy range name = do
  result <- State.gets (HashMap.lookup name . localTypes)
  maybe
    (typeError (CannotFindType range name))
    (pure . TyRef range)
    result

inferTy :: MonadTyper m => Type Normal -> m Ty
inferTy t = track (InInferTy t) $ do
    (ty, unbounded) <- go t
    pure (foldl (flip $ TyForall (getPos t)) ty unbounded)
  where
    go :: MonadTyper m => Type Normal -> m (Ty, Set Name)
    go ty = track (InInferTy ty) $ case ty of
      TId name _       -> do
        ty' <- getTy undefined undefined
        pure (ty', Set.empty)
      TPoly (Expr.Name ident loc) _         ->
        pure (TyNamed loc ident, Set.singleton ident)
      TArrow ty' ty'' loc' -> do
        (rTy, setA) <- go ty'
        (rTy', setB) <- go ty''
        pure (TyFun loc' rTy rTy', Set.union setA setB)
      TForall (Expr.Name ident _) ty' loc' -> do
        (rTy', set) <- go ty'
        pure (TyForall loc' ident rTy', Set.delete ident set)
      _  -> error "Not implemented yet"

module Typer.Context where

import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Control.Monad.State as ST
import qualified Control.Monad.Except as ER

import Typer.Types

data Ctx =
    Ctx { ctxEnv   :: Map Id Ty
        , ctxNames :: Set Id
        , ctxTypes :: Map Id Kind
        , lvl      :: Lvl
        }

data VarCtx =
    VarCtx { ctxHoles :: Map Lvl Hole
           , nameGen  :: Int
           }

type TypeState m = (ST.MonadState VarCtx m, ER.MonadError String m)

-- Some helper functions

empty :: Ctx
empty = Ctx { ctxEnv = Map.empty, ctxNames = Set.empty, ctxTypes = Map.empty, lvl = 0 }

addKey :: Ctx -> Id -> Ctx
addKey ctx k = ctx { ctxNames = Set.insert k (ctxNames ctx) }

insert :: Ctx -> Id -> Ty -> Ctx
insert ctx k v = ctx { ctxEnv = Map.insert k v (ctxEnv ctx) }

newIdLvl :: TypeState m => m Int
newIdLvl = ST.state $ \s -> ( nameGen s, s { nameGen = nameGen s + 1 })

addTy :: Ctx -> Ctx
addTy ctx = ctx { lvl = lvl ctx + 1 }

createHole :: TypeState m => Lvl -> m Lvl
createHole scope = do
    name <- newIdLvl
    ST.state $ \s -> ( name
                     , s { ctxHoles = Map.insert name (Empty scope) (ctxHoles s) })

getHole :: TypeState m => Lvl -> m Hole
getHole lvl' =
    ST.gets (Map.lookup lvl' . ctxHoles)
    >>= maybe (ER.throwError $ "Cannot find hole '" ++ show lvl' ++ "'") pure

setHole :: TypeState m => Lvl -> Hole -> m ()
setHole k v = ST.modify (\s -> s { ctxHoles = Map.insert k v (ctxHoles s)})

subst :: TypeState m => Id -> Ty -> Ty -> m Ty
subst name repla = \case
  TyCon txt       | info txt == name -> pure repla
                  | otherwise        -> pure $ TyCon txt
  TyForall txt ty | txt == name      -> pure $ TyForall txt ty
                  | otherwise        -> TyForall txt <$> subst name repla ty
  TyFun ty ty' -> TyFun <$> subst name repla ty <*> subst name repla ty'
  TyApp ty ty' -> TyApp <$> subst name repla ty <*> subst name repla ty'
  TyBound n   -> pure $ TyBound n
  TyExists n  -> do
      hole <- getHole n
      case hole of
        Empty _    -> pure ()
        Filled ty' -> subst name repla ty' >>= setHole n . Filled
      pure $ TyExists n

instantiate :: TypeState m => Ctx -> Id -> Ty -> m Ty
instantiate ctx name ty = do
    hole <- createHole (lvl ctx)
    subst name (TyExists hole) ty

findKind :: TypeState m => Ctx -> Id -> m Kind
findKind ctx id' = 
    maybe (ER.throwError $ "Cannot find type '" ++ show id' ++ "'") 
          pure 
          (Map.lookup id' (ctxTypes ctx)) 

executeT :: ST.StateT VarCtx (ER.Except String) a -> Either String a
executeT st = ER.runExcept $ ST.evalStateT st (VarCtx Map.empty 0)
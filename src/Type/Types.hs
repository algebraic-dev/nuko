module Type.Types where 

import qualified Data.Set as Set ( Set, delete, empty, singleton, union )

type TypeId = String
data TypeKind = Poly | Mono 

data RType :: TypeKind -> * where 
  TyUnit   :: RType a 
  TyAlpha  :: TypeId -> RType a 
  TyExists :: TypeId -> RType a 
  TyFun    :: RType a -> RType a -> RType a 
  TyForall :: TypeId -> RType 'Poly -> RType 'Poly 

deriving instance Eq (RType a)

instance Show (RType a) where 
  show = \case 
    TyUnit -> "()"
    TyAlpha s -> s
    TyExists s -> '^' : s
    TyFun ty@(TyForall _ _) ty' -> "(" ++ show ty ++ ") -> " ++ show ty'
    TyFun ty@(TyFun _ _) ty' -> "(" ++ show ty ++ ") -> " ++ show ty'
    TyFun ty ty' -> show ty ++ " -> " ++ show ty'
    TyForall s ty -> "∀ " ++ s ++ " . " ++ show ty


freeVars :: RType a -> Set.Set TypeId 
freeVars = \case 
  TyUnit       -> Set.empty
  TyAlpha i    -> Set.singleton i
  TyExists i   -> Set.singleton i 
  TyFun a b    -> Set.union (freeVars a) (freeVars b)
  TyForall b t -> Set.delete b (freeVars t) 

occursIn :: TypeId -> RType a -> Bool
occursIn name = \case 
  TyUnit        -> False
  TyAlpha s     -> s == name 
  TyExists s    -> s == name
  TyFun ty ty'  -> occursIn name ty || occursIn name ty'
  TyForall s ty -> s == name || occursIn name ty

substitute :: TypeId -> RType a -> RType a -> RType a
substitute from to = \case 
  TyUnit        -> TyUnit
  TyFun ty ty'  -> TyFun (substitute from to ty) (substitute from to ty')
  TyAlpha s      | s == from -> to   
                 | otherwise -> TyAlpha s 
  TyExists s     | s == from -> to 
                 | otherwise -> TyExists s
  TyForall s ty  | s == from -> TyForall s ty 
                 | otherwise -> TyForall s (substitute from to ty) 

toMono :: RType 'Poly -> Maybe (RType 'Mono) 
toMono = \case 
  TyUnit -> Just TyUnit
  TyAlpha s -> Just $ TyAlpha s
  TyExists s -> Just $ TyExists s
  TyFun ty ty' -> TyFun <$> toMono ty <*> toMono ty' 
  TyForall _ _ -> Nothing

toPoly :: RType 'Mono -> RType 'Poly
toPoly = \case
  TyUnit -> TyUnit
  TyAlpha s -> TyAlpha s
  TyExists s -> TyExists s
  TyFun ty ty' -> TyFun (toPoly ty) (toPoly ty')

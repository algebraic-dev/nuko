module Type.Types where 

import qualified Data.Set as Set ( Set, delete, empty, singleton, union )

type TypeId = String
data TypeKind = Poly | Mono 

data Type :: TypeKind -> * where 
  TyUnit   :: Type a 
  TyAlpha  :: TypeId -> Type a 
  TyExists :: TypeId -> Type a 
  TyFun    :: Type a -> Type a -> Type a 
  TyForall :: TypeId -> Type 'Poly -> Type 'Poly 

deriving instance Eq (Type a)

instance Show (Type a) where 
  show = \case 
    TyUnit -> "()"
    TyAlpha s -> s
    TyExists s -> '^' : s
    TyFun ty@(TyForall _ _) ty' -> "(" ++ show ty ++ ") -> " ++ show ty'
    TyFun ty@(TyFun _ _) ty' -> "(" ++ show ty ++ ") -> " ++ show ty'
    TyFun ty ty' -> show ty ++ " -> " ++ show ty'
    TyForall s ty -> "∀ " ++ s ++ " . " ++ show ty


freeVars :: Type a -> Set.Set TypeId 
freeVars = \case 
  TyUnit       -> Set.empty
  TyAlpha i    -> Set.singleton i
  TyExists i   -> Set.singleton i 
  TyFun a b    -> Set.union (freeVars a) (freeVars b)
  TyForall b t -> Set.delete b (freeVars t) 

occursIn :: TypeId -> Type a -> Bool
occursIn name = \case 
  TyUnit        -> False
  TyAlpha s     -> s == name 
  TyExists s    -> s == name
  TyFun ty ty'  -> occursIn name ty || occursIn name ty'
  TyForall s ty -> s == name || occursIn name ty

substitute :: TypeId -> Type a -> Type a -> Type a
substitute from to = \case 
  TyUnit        -> TyUnit
  TyFun ty ty'  -> TyFun (substitute from to ty) (substitute from to ty')
  TyAlpha s      | s == from -> to   
                 | otherwise -> TyAlpha s 
  TyExists s     | s == from -> to 
                 | otherwise -> TyExists s
  TyForall s ty  | s == from -> TyForall s ty 
                 | otherwise -> TyForall s (substitute from to ty) 

toMono :: Type 'Poly -> Maybe (Type 'Mono) 
toMono = \case 
  TyUnit -> Just TyUnit
  TyAlpha s -> Just $ TyAlpha s
  TyExists s -> Just $ TyExists s
  TyFun ty ty' -> TyFun <$> toMono ty <*> toMono ty' 
  TyForall _ _ -> Nothing

toPoly :: Type 'Mono -> Type 'Poly
toPoly = \case
  TyUnit -> TyUnit
  TyAlpha s -> TyAlpha s
  TyExists s -> TyExists s
  TyFun ty ty' -> TyFun (toPoly ty) (toPoly ty')

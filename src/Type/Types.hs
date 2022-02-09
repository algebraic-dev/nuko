module Type.Types where 

import Data.Text (Text)
import Syntax.Bounds (Bounds)

import qualified Data.Set as Set
import Data.Function (on)

type TypeId = Text
data TypeKind = Poly | Mono 

data Type :: TypeKind -> * where 
  TyAlpha  :: Maybe Bounds -> TypeId -> Type a 
  TyExists :: Maybe Bounds -> TypeId -> Type a 
  TyForall :: Maybe Bounds -> TypeId -> Type 'Poly -> Type 'Poly
  TyFun    :: Maybe Bounds -> Type a -> Type a -> Type a

instance Eq (Type a) where 
  TyAlpha _ a    == TyAlpha _ b      = a == b
  TyExists _ a   == TyExists _ b     = a == b
  TyForall _ a b == TyForall _ a' b' = a == a' && b == b' 
  TyFun _ a b    == TyFun _ a' b'    = a == a' && b == b'
  _              == _                = False

substitute :: TypeId -> Type a -> Type a -> Type a
substitute from to = \case 
  TyFun b ty ty'   -> TyFun b (substitute from to ty) (substitute from to ty')
  TyAlpha b txt     | txt == from -> to
                    | otherwise   -> TyAlpha b txt
  TyExists b txt    | txt == from -> to 
                    | otherwise   -> TyExists b txt
  TyForall b txt ty | txt == from -> TyForall b txt ty 
                    | otherwise   -> TyForall b txt (substitute from to ty)   

tyFreeVars :: Type a -> Set.Set Text 
tyFreeVars = \case 
  TyAlpha _ txt     -> Set.singleton txt
  TyExists _ txt    -> Set.singleton txt
  TyForall _ txt ty -> Set.delete txt (tyFreeVars ty)
  TyFun _ ty ty'    -> (Set.union `on` tyFreeVars) ty ty'

toPoly :: Type 'Mono -> Type 'Poly 
toPoly = \case 
  TyAlpha m_bo txt  -> TyAlpha m_bo txt
  TyExists m_bo txt -> TyExists m_bo txt
  TyFun m_bo ty ty' -> TyFun m_bo (toPoly ty) (toPoly ty')

toMono :: Type 'Poly -> Maybe (Type 'Mono)
toMono = \case 
  TyAlpha m_bo txt  -> Just $ TyAlpha m_bo txt
  TyExists m_bo txt -> Just $ TyExists m_bo txt
  TyFun m_bo ty ty' -> TyFun m_bo <$> toMono ty <*> toMono ty'
  TyForall {}       -> Nothing

occoursIn :: TypeId -> Type 'Poly -> Bool 
occoursIn name = \case 
  TyAlpha _ txt     -> txt == name
  TyExists _ txt    -> txt == name
  TyForall _ txt ty -> txt /= name && occoursIn name ty
  TyFun _ ty ty'    -> occoursIn name ty || occoursIn name ty'


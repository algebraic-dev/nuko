module Typer.Types where 

import Data.IORef (IORef)
import Data.Text (Text)
import Syntax.Range (Loc)
import qualified Data.Text as Text
import GHC.IO (unsafePerformIO)
import GHC.IORef (readIORef)

type Lvl = Int
type Name = Text

data Hole ty
  = Empty Name Lvl
  | Filled ty
  deriving Show 

-- Mutable refenreces...
type TyHole = IORef (Hole Ty)

data Ty
  = TyForall Loc Name Ty -- Universal quantifier
  | TyHole Loc TyHole -- Holes are like meta variables
  | TyRigid Loc Text Lvl -- Type variables that unified with itself and have it's scope
  | TyNamed Loc Name -- Bounded type variables and normal types
  | TyFun Loc Ty Ty -- Function type

instance Show Ty where 
  show = \case 
    TyFun _ (ty@TyForall {}) ty' -> "(" ++ show ty ++ ") -> " ++ show ty'
    TyFun _ (ty@TyFun {}) ty' -> "(" ++ show ty ++ ") -> " ++ show ty'
    TyForall _ na ty  -> "∀" ++ Text.unpack na ++ ". " ++ show ty 
    TyRigid _ txt lvl -> "^" ++ Text.unpack txt ++ show lvl 
    TyNamed _ name    -> Text.unpack name
    TyFun _ ty ty'    -> show ty ++ " -> " ++ show ty'
    TyHole _ hole     -> 
      case unsafePerformIO (readIORef hole) of 
        Empty n h -> "?" ++ Text.unpack n ++ show h
        Filled t -> show t 

instance Eq Ty where 
    (TyForall _ a b) == (TyForall _ a' b') = a == a' && b == b'
    (TyHole _ b) == (TyHole _ b') = b == b'
    (TyRigid _ _ b) == (TyRigid _ _ b') = b == b'
    (TyNamed _ b) == (TyNamed _ b') = b == b'
    (TyFun _ a b) == (TyFun _ a' b') = a == a' && b == b'
    _ == _ = False

getTyPos :: Ty -> Loc 
getTyPos = \case 
  TyForall loc _ _ -> loc
  TyFun loc _ _ -> loc
  TyRigid loc _ _ -> loc
  TyHole loc _ -> loc
  TyNamed loc _ -> loc

substitute :: Name -> Ty -> Ty -> Ty
substitute from to = \case
  TyForall loc binder body
    | binder == from -> TyForall loc binder body
    | otherwise -> TyForall loc binder (substitute from to body)
  TyFun loc a b -> TyFun loc (substitute from to a) (substitute from to b)
  TyNamed loc name
    | name == from -> to
    | otherwise -> TyNamed loc name
  TyHole loc hole ->
    case unsafePerformIO (readIORef hole) of
      Empty _ _ -> TyHole loc hole
      Filled ty -> substitute from to ty
  TyRigid loc text r -> TyRigid loc text r


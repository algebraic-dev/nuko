module Typer.Types (
  Hole(..),
  Lvl,
  Name,
  Ty(..),
  getTyPos,
  substitute,
  isFilled,
  getFilled,
  TyHole
) where

import Data.IORef   (IORef)
import Data.Text    (Text)
import Syntax.Range (Loc)
import GHC.IO       (unsafePerformIO)
import GHC.IORef    (readIORef)
import Data.Kind    (Type)

import qualified Data.Text as Text

type Lvl = Int
type Name = Text

data Hole ty
  = Empty Name Lvl
  | Filled ty
  deriving Show

-- Mutable refenreces...
type TyHole = IORef (Hole Ty)

data Ty :: Type where
  TyForall :: Loc -> Name -> Ty  -> Ty
  TyHole   :: Loc -> TyHole      -> Ty
  TyRigid  :: Loc -> Text -> Lvl -> Ty
  TyNamed  :: Loc -> Name        -> Ty
  TyFun    :: Loc -> Ty -> Ty    -> Ty
  TyRef    :: Loc -> Ty -> Ty

instance Show Ty where
  show = \case
    TyFun _ (ty@(TyHole _ hole)) ty' ->
      case unsafePerformIO (readIORef hole) of
        Filled (TyFun {})  -> "(" ++ show ty ++ ") -> " ++ show ty'
        _ ->  show ty ++ " -> " ++ show ty'
    TyFun _ (ty@TyForall {}) ty' -> "(" ++ show ty ++ ") -> " ++ show ty'
    TyFun _ (ty@TyFun {}) ty' -> "(" ++ show ty ++ ") -> " ++ show ty'
    TyForall _ na ty  -> "∀" ++ Text.unpack na ++ ". " ++ show ty
    TyRigid _ txt lvl -> "^" ++ Text.unpack txt ++ show lvl
    TyNamed _ name    -> Text.unpack name
    TyFun _ ty ty'    -> show ty ++ " -> " ++ show ty'
    TyRef _ ty        -> show ty
    TyHole _ hole     ->
      case unsafePerformIO (readIORef hole) of
        Empty n _ -> Text.unpack n
        Filled t -> show t

instance Eq Ty where
    (TyForall _ a b) == (TyForall _ a' b') = a == a' && b == b'
    (TyHole _ b) == (TyHole _ b') = b == b'
    (TyRigid _ _ b) == (TyRigid _ _ b') = b == b'
    (TyNamed _ b) == (TyNamed _ b') = b == b'
    (TyFun _ a b) == (TyFun _ a' b') = a == a' && b == b'
    TyRef _ ty  ==  b = ty == b
    ty  ==  TyRef _ b = ty == b
    _ == _ = False

getTyPos :: Ty -> Loc
getTyPos = \case
  TyForall loc _ _ -> loc
  TyFun loc _ _ -> loc
  TyRigid loc _ _ -> loc
  TyHole loc _ -> loc
  TyNamed loc _ -> loc
  TyRef loc _ -> loc

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
  TyRef loc ty -> TyRef loc (substitute from to ty)

isFilled :: TyHole -> Bool
isFilled hole =
  case unsafePerformIO (readIORef hole) of
    Empty _ _ -> False
    Filled _  -> True

getFilled :: TyHole -> Ty
getFilled hole =
    case unsafePerformIO (readIORef hole) of
      Empty _ _ -> error "Empty lol"
      Filled t  ->t

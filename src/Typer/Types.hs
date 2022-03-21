module Typer.Types where
import Data.Text (Text, unpack)

type Id = Text
type Lvl = Int

data Kind = Star | KFun Kind Kind
    deriving Eq

data Kinded a = Kinded { info :: a, kind :: Kind }
    deriving Eq

data Hole
    = Empty Lvl
    | Filled Ty
    deriving Eq

data Ty
    = TyCon (Kinded Id)
    | TyFun Ty Ty
    | TyApp Ty Ty
    | TyForall Id Ty
    | TyBound Lvl
    | TyExists Lvl
    deriving Eq

instance Show Kind where 
    show = \case 
      Star -> "*"
      KFun ki@KFun {} ki' -> "( " ++ show ki ++ ") -> " ++ show ki'
      KFun a b -> show a ++ " -> " ++ show b

instance Show Ty where
    show = \case
      TyCon (Kinded s Star)     -> unpack s
      TyCon (Kinded s k)        -> "(" ++ unpack s ++ " : " ++ show k ++ ")"
      TyFun ty@TyFun {} ty'     -> "(" ++ show ty ++ ") -> " ++ show ty'
      TyFun ty@TyForall {} ty'  -> "(" ++ show ty ++ ") -> " ++ show ty'
      TyApp a b                 -> "(" ++ show a ++ " " ++ show b ++ ")"
      TyFun ty ty'  -> show ty ++ " -> " ++ show ty'
      TyForall s ty -> "∀ " ++ unpack s ++ ". " ++ show ty
      TyBound n    -> show n
      TyExists s   -> show s

kindArgsLen :: Kind -> Int
kindArgsLen (KFun _ b) = 1 + kindArgsLen b
kindArgsLen Star       = 0

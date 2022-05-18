module Nuko.Typer.Types (
     Ty(..),
     Hole(..)
) where

import Data.Text         (Text)
import Data.IORef        (IORef)
import GHC.IO            (unsafePerformIO)
import GHC.IORef         (readIORef)
import Data.Kind         (Type)
import Nuko.Syntax.Range (Pos)

import qualified Data.Text as Text

type Lvl = Int

type Name = Text

type TyHole = IORef (Hole Ty)

data Hole ty
  = Empty Name Lvl
  | Filled ty
  deriving Show

data Ty :: Type where
  TyHole   :: Pos -> TyHole      -> Ty
  TyNamed  :: Pos -> Name        -> Ty
  TyFun    :: Pos -> Ty -> Ty    -> Ty
  TyRef    :: Pos -> Ty -> Ty

instance Show Ty where
  show = \case
    TyFun _ (ty@(TyHole _ hole)) ty' ->
      case unsafePerformIO (readIORef hole) of
        Filled (TyFun {})  -> "(" ++ show ty ++ ") -> " ++ show ty'
        _ ->  show ty ++ " -> " ++ show ty'
    TyFun _ (ty@TyFun {}) ty' -> "(" ++ show ty ++ ") -> " ++ show ty'
    TyFun _ ty ty'    -> show ty ++ " -> " ++ show ty'
    TyRef _ ty        -> show ty
    TyNamed _ t       -> show t
    TyHole _ hole     ->
      case unsafePerformIO (readIORef hole) of
        Empty n _ -> Text.unpack n
        Filled t -> show t
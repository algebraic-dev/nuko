module Typer.Types where

import Data.IORef (IORef, readIORef)
import Data.Text (Text)

import qualified Data.Text   as Text
import GHC.IO (unsafePerformIO)
import Syntax.Bounds (Bounds)
import Control.Applicative ( Alternative((<|>)) )

-- Lol this is helpful...

subs :: String
subs = "₀₁₂₃₄₅₆₇₈₉"

toSubscript :: Int -> String
toSubscript num 
    | num == 0  = [head subs]
    | otherwise =  reverse (go num)
  where
    go :: Int -> String
    go x
      | x < 0 = toSubscript (x * (-1))
      | x == 0 = ""
      | otherwise =
        let (q, r) = x `quotRem` 10 in
        (subs !! r) : go q

data EvalStatus = Pure | Eval

type Lvl = Int

data Hole a
  = Empty Lvl
  | Filled a
  deriving Eq

data Kind :: EvalStatus -> * where
  Star  :: Kind s
  KFun  :: Kind s -> Kind s -> Kind s
  KGen  :: Lvl    -> Kind 'Pure
  KHole :: IORef (Hole (Kind 'Eval)) -> Kind 'Eval
  KLoc  :: Bounds -> Kind 'Eval -> Kind 'Eval

data KindScheme = KindScheme { kindCount :: Lvl, kindTy :: Kind 'Pure }

data Type :: EvalStatus -> * where
  TyNamed   :: Text   -> Type s
  TyFun     :: Type s -> Type s -> Type s
  TyApp     :: Kind s -> Type s -> Type s -> Type s
  TyForall  :: Text   -> Type s -> Type s
  TyBounded :: Lvl    -> Type s
  TyHole    :: IORef (Hole (Type 'Eval)) -> Type 'Eval
  TyLoc     :: Bounds -> Type 'Eval -> Type 'Eval

instance Eq (Kind s) where
  f == s = case (f,s) of
    (Star,     Star) -> True 
    (KFun a b, KFun a' b') -> a == a' && b == b'
    (KGen a,   KGen b) -> a == b 
    (KHole a,  KHole b) -> a == b 
    (KLoc _ a, KLoc _ b)  -> a == b 
    (_,        _) -> False

instance Eq (Type a) where 
  f == s = case (f, s) of
    (TyNamed a, TyNamed b) -> a == b 
    (TyFun a b, TyFun a' b') -> a == a' && b == b'
    (TyApp k a b, TyApp k' a' b') ->  a == a' && b == b' && k == k'
    (TyForall t a, TyForall t' a') -> t == t' && a == a'
    (TyHole a, TyHole b) -> a == b 
    (TyLoc _ a, TyLoc _ b)  -> a == b 
    (_, _) -> False

instance Show (Type a) where
  show = \case
    TyBounded s -> "b" ++ toSubscript s
    TyLoc _ a -> show a
    TyNamed s -> Text.unpack s
    TyFun t@TyForall {} t'  -> concat [ "(", show t, ") -> ", show t' ]
    TyFun ty@TyFun {} ty'   -> concat [ "(", show ty, ") -> ", show ty' ]
    TyFun (TyHole ty) ty' ->
     case unsafePerformIO (readIORef ty) of
       Empty lvl -> concat ["h" ++ toSubscript lvl, " -> ", show ty']
       Filled ty'' -> show (TyFun ty'' ty')
    TyFun ty ty'            -> concat [ show ty, " -> ", show ty' ]
    TyApp _ ty ty'          -> concat [ show ty, " ", show ty' ]
    TyForall n ty           -> concat [ "∀", Text.unpack n, ".", show ty ]
    TyHole hole ->
        case unsafePerformIO (readIORef hole) of
            Empty lvl -> "h" ++ toSubscript lvl
            Filled ki -> show ki

instance Show (Kind a) where
  show = \case
    KGen s   -> "'" ++ show s
    KLoc _ s -> show s
    Star -> "*"
    KFun ki@KFun {} ki' -> concat ["(", show ki, ") -> ", show ki']
    KFun (KHole ki) ki' ->
     case unsafePerformIO (readIORef ki) of
       Empty lvl -> concat ["k" ++ toSubscript lvl, " -> ", show ki']
       Filled ki'' -> show (KFun ki'' ki')
    KFun ki ki' -> concat [show ki, " -> ", show ki']
    KHole ir ->
     case unsafePerformIO (readIORef ir) of
       Empty lvl -> "k" ++ toSubscript lvl
       Filled ki -> show ki

getPosKind :: Kind a -> Maybe Bounds
getPosKind = \case
  Star -> Nothing
  KFun ki ki' -> getPosKind ki <|> getPosKind ki'
  KGen _ -> Nothing
  KHole ir ->
    case unsafePerformIO (readIORef ir) of
      Empty   _ -> Nothing
      Filled ki -> getPosKind ki
  KLoc bo _ -> Just bo

getPos :: Type a -> Maybe Bounds
getPos = \case
  TyBounded _ -> Nothing 
  TyNamed _ -> Nothing
  TyFun ty ty' -> getPos ty <|> getPos ty'
  TyApp _ ty ty' -> getPos ty <|> getPos ty'
  TyForall _ ty -> getPos ty
  TyHole ir -> 
    case unsafePerformIO (readIORef ir) of
      Empty   _ -> Nothing
      Filled ki -> getPos ki
  TyLoc _ ty -> getPos ty
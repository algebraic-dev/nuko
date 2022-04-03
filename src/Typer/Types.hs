module Typer.Types (
  Kind(..),
  TType(..),
  KindScheme(..),
  getPos,
  EvalStatus(..),
  Hole(..),
  Loc(..),
  Lvl
) where

import Data.IORef          (IORef, readIORef)
import Data.Text           (Text)
import Data.Kind           (Type)
import GHC.IO              (unsafePerformIO)
import Syntax.Range       (Range, HasPosition (getPos))
import Control.Applicative (Alternative((<|>)) )

import qualified Data.Text   as Text

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

data Loc = Ghost | Loc Range

data Kind :: EvalStatus -> Type where
  Star  :: Loc -> Kind s
  KFun  :: Loc -> Kind s -> Kind s -> Kind s
  KGen  :: Loc -> Lvl    -> Kind 'Pure
  KHole :: Loc -> IORef (Hole (Kind 'Eval)) -> Kind 'Eval

data KindScheme = KindScheme { kindCount :: Lvl, kindTy :: Kind 'Pure }

data TType :: EvalStatus -> Type where
  TyNamed   :: Text   -> TType s
  TyFun     :: TType s -> TType s -> TType s
  TyApp     :: Kind s -> TType s -> TType s -> TType s
  TyForall  :: Text   -> TType s -> TType s
  TyRigid   :: Lvl    -> TType s
  TyHole    :: IORef (Hole (TType 'Eval)) -> TType 'Eval

instance Eq (Kind s) where
  f == s = case (f,s) of
    (Star _,     Star _) -> True 
    (KFun _ a b, KFun _ a' b') -> a == a' && b == b'
    (KGen _ a,   KGen _ b) -> a == b 
    (KHole _ a,  KHole _ b) -> a == b 
    (_,        _) -> False

instance Eq (TType a) where 
  f == s = case (f, s) of
    (TyNamed a, TyNamed b) -> a == b 
    (TyFun a b, TyFun a' b') -> a == a' && b == b'
    (TyApp k a b, TyApp k' a' b') ->  a == a' && b == b' && k == k'
    (TyForall t a, TyForall t' a') -> t == t' && a == a'
    (TyHole a, TyHole b) -> a == b 
    (_, _) -> False

instance Show (TType a) where
  show = \case
    TyRigid s -> "b" ++ toSubscript s
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
    KGen _ s   -> "'" ++ show s
    Star _ -> "*"
    KFun _ ki@KFun {} ki' -> concat ["(", show ki, ") -> ", show ki']
    KFun b (KHole _ ki) ki' ->
     case unsafePerformIO (readIORef ki) of
       Empty lvl -> concat ["k" ++ toSubscript lvl, " -> ", show ki']
       Filled ki'' -> show (KFun b ki'' ki')
    KFun _ ki ki' -> concat [show ki, " -> ", show ki']
    KHole _ ir ->
     case unsafePerformIO (readIORef ir) of
       Empty lvl -> "k" ++ toSubscript lvl
       Filled ki -> show ki

instance HasPosition (Kind a) where 
  getPos = \case
    Star (Loc pos) -> pos
    KFun (Loc a) ki ki' -> a
    KGen (Loc p) _ -> p
    KHole p ir ->
      case unsafePerformIO (readIORef ir) of
        Empty   _ -> p
        Filled ki -> getPos ki
    _ -> error "be" 
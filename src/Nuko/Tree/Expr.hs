module Nuko.Tree.Expr (
  Literal(..),
  Path(..),
  Pat(..),
  Expr(..),
  NoExt(..),
  Name(..),
  XLInt,
  XLStr,
  XPWild,
  XPId,
  XPExt,
  XLit,
  XLam,
  XCall,
  XLower,
  XUpper,
  XAccessor,
  XIf,
  XCase,
  XName,
  XExt,
) where

import Data.Text          (Text)
import Data.List.NonEmpty (NonEmpty)

data NoExt = NoExt

data Name x = Name Text (XName x)

data Path a x = Path { path :: [Name x], final :: a }

data Literal x
  = LStr Text (XLInt x)
  | LInt Int (XLStr x)

-- | Describes patterns that are useful for things like
--  in case and let bindings.
data Pat x
  = PWild (XPWild x)
  | PId (Name x) (XPId x)
  | PCons (Path (Name x) x) [Pat x] (XPId x)
  | PExt (XPExt x)

data Expr x
  = Lit (Literal x) (XLit x) -- Literal
  | Lam (Pat (Name x)) (XLam x) -- Lambdas / Anonymous function
  | Call (Expr x) (NonEmpty (Expr x)) (XCall x) -- Function call
  | Lower (Path (Name x) x) (XLower x) -- Lower cased ids
  | Upper (Path (Name x) x) (XUpper x) -- Upper cased ids
  | Accessor (Expr x) (Name x) (XAccessor x) -- Record fields like A.b.c where c is the Name
  | If (Expr x) (Expr x) (Expr x) (XIf x) -- If then else statement
  | Case (Expr x) [((Pat x), (Expr x))] (XCase x) -- Case of statement
  | Ext !(XExt x)

type family XName x

type family XLInt x
type family XLStr x

type family XPWild x
type family XPId x
type family XPExt x

type family XLit x
type family XLam x
type family XCall x
type family XLower x
type family XUpper x
type family XAccessor x
type family XIf x
type family XCase x
type family XExt x

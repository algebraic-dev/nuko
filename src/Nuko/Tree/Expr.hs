module Nuko.Tree.Expr (
  Literal(..),
  Path(..),
  Pat(..),
  Expr(..),
  NoExt(..),
  Name(..),
  Block(..),
  Type(..),
  Var(..),
  XLInt,
  XLStr,
  XPWild,
  XPId,
  XPCons,
  XPLit,
  XPExt,
  XLit,
  XLam,
  XApp,
  XLower,
  XUpper,
  XAccessor,
  XAnn,
  XPAnn,
  XIf,
  XCase,
  XName,
  XBlock,
  XExt,
  XVar,
  XTId,
  XTPoly,
  XTCons,
  XTArrow,
  XTForall,
  XPaExt,
  XNaExt,
  XPath,
) where

import Data.Text          (Text)
import Data.List.NonEmpty (NonEmpty)

data NoExt = NoExt

data Name x = Name Text (XName x)

data Path x
  = Path [Name x] (Name x) (XPath x)
  | PaExt (XPaExt x)

data Type x
  -- | Type identifier
  = TId (Path x) (XTId x)
  -- | Polymorphic type identifier (everything that is lower case)
  | TPoly (Name x) (XTPoly x)
  -- | Type constructor
  | TCons (Path x) (NonEmpty (Type x)) (XTCons x)
  -- | Arrow type
  | TArrow  (Type x) (Type x) (XTArrow x)
  | TForall (Name x) (Type x) (XTForall x)

data Literal x
  = LStr Text (XLInt x)
  | LInt Int (XLStr x)

-- | Describes patterns that are useful for things like
--  in case and let bindings.
data Pat x
  = PWild (XPWild x)
  | PId (Name x) (XPId x)
  | PCons (Path x) [Pat x] (XPCons x)
  | PLit (Literal x) (XPLit x)
  | PAnn (Pat x) (Type x) (XPAnn x)
  | PExt (XPExt x)

data Var x
  = Var (Pat x) (Expr x) (XVar x)

data Block x
  = BlBind (Expr x) (Block x)
  | BlVar (Var x) (Block x)
  | BlEnd (Expr x)

data Expr x
  = Lit (Literal x) (XLit x) -- Literal
  | Lam (Pat x) (Expr x) (XLam x) -- Lambdas / Anonymous function
  | App (Expr x) (NonEmpty (Expr x)) (XApp x) -- Function call
  | Lower (Path x) (XLower x) -- Lower cased ids
  | Upper (Path x) (XUpper x) -- Upper cased ids
  | Accessor (Expr x) (Name x) (XAccessor x)      -- Record fields like A.b.c where c is the Name
  | If (Expr x) (Expr x) (Maybe (Expr x)) (XIf x) -- If then else statement
  | Case (Expr x) [((Pat x), (Expr x))] (XCase x) -- Case of statement
  | Ann (Expr x) (Type x) (XAnn x)
  | Block (Block x) (XBlock x)
  | Ext !(XExt x)

type family XVar x

type family XName x

type family XLInt x
type family XLStr x

type family XTId x
type family XTPoly x
type family XTCons x
type family XTArrow x
type family XTForall x

type family XPWild x
type family XPId x
type family XPAnn x
type family XPCons x
type family XPExt x
type family XPLit x

type family XLit x
type family XLam x
type family XAnn x
type family XApp x
type family XLower x
type family XUpper x
type family XAccessor x
type family XIf x
type family XCase x
type family XBlock x
type family XExt x

type family XPath x
type family XPaExt x
type family XNaExt x

instance Show NoExt where
  show _ = "(.)"

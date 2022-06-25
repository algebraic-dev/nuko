module Nuko.Tree.Expr (
  Literal(..),
  Path(..),
  Pat(..),
  Expr(..),
  NoExt(..),
  Name(..),
  Block(..),
  Ty(..),
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
  XField,
  XAnn,
  XPAnn,
  XIf,
  XMatch,
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

import Relude ( Show, Int, Maybe, NonEmpty, Text )

data NoExt = NoExt deriving Show

data Name x = Name Text !(XName x)

data Path x
  = Path [Name x] (Name x) !(XPath x)
  | PaExt !(XPaExt x)

data Ty x
  = TId (Path x) !(XTId x)
  | TPoly (Name x) !(XTPoly x)
  | TCons (Path x) (NonEmpty (Ty x)) !(XTCons x)
  | TArrow  (Ty x) (Ty x) !(XTArrow x)
  | TForall (Name x) (Ty x) !(XTForall x)

data Literal x
  = LStr Text !(XLInt x)
  | LInt Int !(XLStr x)

-- | Describes patterns that are useful for things like
--  in Match and let bindings.
data Pat x
  = PWild !(XPWild x)
  | PId (Name x) !(XPId x)
  | PCons (Path x) [Pat x] !(XPCons x)
  | PLit (Literal x) !(XPLit x)
  | PAnn (Pat x) (Ty x) !(XPAnn x)
  | PExt !(XPExt x)

data Var x
  = Var (Pat x) (Expr x) !(XVar x)

data Block x
  = BlBind (Expr x) (Block x)
  | BlVar (Var x) (Block x)
  | BlEnd (Expr x)

data Expr x
  = Lit (Literal x) !(XLit x)
  | Lam (Pat x) (Expr x) !(XLam x)
  | App (Expr x) (NonEmpty (Expr x)) !(XApp x)
  | Lower (Path x) !(XLower x)
  | Upper (Path x) !(XUpper x)
  | Field (Expr x) (Name x) !(XField x)
  | If (Expr x) (Expr x) (Maybe (Expr x)) !(XIf x)
  | Match (Expr x) (NonEmpty (Pat x, Expr x)) !(XMatch x)
  | Ann (Expr x) (Ty x) !(XAnn x)
  | Block (Block x) !(XBlock x)
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
type family XField x
type family XIf x
type family XMatch x
type family XBlock x
type family XExt x

type family XPath x
type family XPaExt x
type family XNaExt x
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Nuko.Tree.Expr where

import Relude

import Nuko.Names  (ConsName, TyName, ValName)
import Pretty.Tree (PrettyTree (prettyTree), Tree (..))

data NoExt = NoExt deriving Show

instance PrettyTree NoExt where prettyTree _ = Node "NoExt" [] []

data RecordBinder val x = RecordBinder {
   rbName :: XName x ValName,
   rbVal  :: val x,
   rbExt  :: !(XRecMono x)
}
-- Abstract Syntax Tree

data Ty x
  = TId (XPath x TyName) !(XTId x)
  | TPoly (XName x TyName) !(XTPoly x)
  | TApp (Ty x) (NonEmpty (Ty x)) !(XTCons x)
  | TArrow  (Ty x) (Ty x) !(XTArrow x)
  | TForall (XName x TyName) (Ty x) !(XTForall x)

data Literal x
  = LStr Text !(XLInt x)
  | LInt Int !(XLStr x)

data Pat x
  = PWild !(XPWild x)
  | PId (XName x ValName) !(XPId x)
  | PCons (XPath x ConsName) [Pat x] !(XPCons x)
  | PLit (Literal x) !(XPLit x)
  | PAnn (Pat x) (XTy x) !(XPAnn x)
  | POr (Pat x) (Pat x) !(XPOr x)
  | PRec (XPath x TyName) [RecordBinder Pat x] (XPRec x)
  | PExt !(XPExt x)

data Var x = Var
  { pat :: Pat x
  , val :: Expr x
  , ext :: !(XVar x)
  }

data Block x
  = BlBind (Expr x) (Block x)
  | BlVar (Var x) (Block x)
  | BlEnd (Expr x)

-- We dont have a CST because i dont like new infix
-- operators that much.
data Operator
  = Add | Sub | Mul | Div
  | Xor | BinOr | BinAnd
  | Or | And | Pipe
  deriving Generic

data Expr x
  = Lit (Literal x) !(XLit x)
  | Lam (Pat x) (Expr x) !(XLam x)
  | App (Expr x) (NonEmpty (Expr x)) !(XApp x)
  | Lower (XPath x ValName) !(XLower x)
  | Upper (XPath x ConsName) !(XUpper x)
  | Field (Expr x) (XName x ValName) !(XField x)
  | RecCreate (XPath x TyName) [RecordBinder Expr x] !(XRecCreate x)
  | RecUpdate (Expr x) [RecordBinder Expr x] !(XRecUpdate x)
  | BinOp Operator (Expr x) (Expr x) !(XBinOp x)
  | If (Expr x) (Expr x) (Expr x) !(XIf x)
  | Match (Expr x) (NonEmpty (Pat x, Expr x)) !(XMatch x)
  | Ann (Expr x) (XTy x) !(XAnn x)
  | Block (Block x) !(XBlock x)
  | Ext !(XExt x)

type family XModName x
type family XIdent x
type family XName x k
type family XPath x k

type family XVar x

type family XTy x

type family XLInt x
type family XLStr x

type family XTId x
type family XTPoly x
type family XTCons x
type family XTArrow x
type family XTForall x

type family XRecBinder x
type family XRecMono x

type family XPWild x
type family XPId x
type family XPAnn x
type family XPCons x
type family XPOr x
type family XPRec x
type family XPExt x

type family XRecUpdate x
type family XRecCreate x
type family XBinOp x
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

instance PrettyTree Operator where

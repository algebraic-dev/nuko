module Nuko.Lowering.NIR (
  Op(..),
  Expr(..),
  Sttm(..),
) where

import Relude.Numeric (Int)
import Nuko.Names     (Ident(..))

data Op
  = Add | Sub | Mul | Div
  | And | Or | Xor
  | Gt | Lt | Ge | Le | Eq

data Expr
  = Const Int
  | Alloc Int
  | Name Ident
  | Call Expr [Expr]
  | ESeq Expr Expr
  | BinOp Expr Expr
  | If Expr Expr Expr

data Sttm
  = Seq Sttm Sttm
  | Run Expr
  | Assign Ident Expr
  | Set Ident Ident
  | Drop Ident
  | NoOp
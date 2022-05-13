module Nuko.Syntax.Ast (Normal) where

import Nuko.Tree.Expr
import Data.Void (Void)
import Nuko.Syntax.Range ( Range, HasPosition(..) )

data Normal

type instance XName Normal = Range
type instance XLInt Normal = Range
type instance XLStr Normal = Range

type instance XPWild Normal = Range
type instance XPId Normal = Range
type instance XPExt Normal = Range

type instance XLit Normal = NoExt
type instance XLam Normal = Range
type instance XCall Normal = Range
type instance XLower Normal = Range
type instance XUpper Normal = Range
type instance XAccessor Normal = Range
type instance XIf Normal = Range
type instance XCase Normal = Range
type instance XExt Normal = Void

instance HasPosition (Name Normal) where
    getPos (Name _ r) = r

instance HasPosition x => HasPosition (Path x Normal) where
    getPos (Path [] f) = getPos f
    getPos (Path (x : _) f) = getPos x <> getPos f

instance HasPosition (Literal Normal) where
    getPos = \case
        LStr _ r -> r
        LInt _ r -> r

instance HasPosition (Expr Normal) where
    getPos = \case
        Lit t _ -> getPos t
        Lam _ r -> r
        Call _ _ r -> r
        Lower _ r -> r
        Upper _ r -> r
        Accessor _ _ r -> r
        If _ _ _ r -> r
        Case _ _ r -> r
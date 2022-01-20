module Syntax.Parser.AST where 

import Data.List.NonEmpty ()
import Syntax.Bounds (Bounds, WithBounds(..))
import Data.Void (Void)
import Syntax.Expr 

data Normal

type instance XName Normal = Bounds

type instance XTSimple Normal = NoExt
type instance XTPoly Normal = NoExt
type instance XTArrow Normal = Bounds
type instance XTCons Normal = Bounds
type instance XTExt Normal = Void

type instance XPWild Normal = Bounds
type instance XPCons Normal = Bounds
type instance XPLit Normal = NoExt
type instance XPExt Normal = Void

type instance XLChar Normal = Bounds
type instance XLString Normal = Bounds
type instance XLInt Normal = Bounds
type instance XLDouble Normal = Bounds
type instance XLExt Normal = Void

type instance XLam Normal = Bounds
type instance XApp Normal = Bounds
type instance XVar Normal = NoExt
type instance XLit Normal = NoExt
type instance XMatch Normal = Bounds
type instance XAssign Normal =  Bounds
type instance XBlock Normal = Bounds
type instance XExt Normal = Void

type instance XTcSum Normal = Bounds
type instance XTcRecord Normal = Bounds
type instance XTcSyn Normal = Bounds
type instance XTcExt Normal = Void

type instance XProg Normal = Bounds
type instance XLet Normal = Bounds
type instance XExport Normal = Bounds
type instance XType Normal = Void

-- Position 

class HasPosition a where 
    getPos :: a -> Bounds

instance HasPosition (Name Normal) where 
    getPos (Name pos _) = pos

instance HasPosition (Type Normal) where 
    getPos (TSimple _ name) = getPos name
    getPos (TPoly _ name) = getPos name
    getPos (TArrow pos _ _) = pos 
    getPos (TCons pos _ _) = pos

instance HasPosition (Pattern Normal) where
    getPos (PWild pos) = pos 
    getPos (PCons pos _ _) = pos 
    getPos (PLit _ lit) = getPos lit 

instance HasPosition (Literal Normal) where 
    getPos (LChar pos _) = pos 
    getPos (LString pos _) = pos 
    getPos (LInt pos _) = pos 
    getPos (LDouble pos _) = pos 

instance HasPosition (Expr Normal) where 
    getPos (Lam pos _ _) = pos 
    getPos (App pos _) = pos 
    getPos (Var _ name) = getPos name 
    getPos (Lit _ lit) = getPos lit 
    getPos (Assign pos _ _) = pos 
    getPos (Match pos _) = pos 
    getPos (Block pos _) = pos 

instance HasPosition (TypeCons Normal) where 
    getPos (TcSum pos _) = pos 
    getPos (TcRecord pos _) = pos 
    getPos (TcSyn _ ty) = getPos ty 

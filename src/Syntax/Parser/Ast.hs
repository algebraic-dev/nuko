module Syntax.Parser.Ast where 

import Data.List.NonEmpty ()
import Syntax.Bounds (Bounds, empty)
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
type instance XPId Normal = NoExt
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
type instance XBinary Normal =  Bounds
type instance XBlock Normal = Bounds
type instance XExt Normal = Void

type instance XTcSum Normal = Bounds
type instance XTcRecord Normal = Bounds
type instance XTcSyn Normal = NoExt
type instance XTcExt Normal = Void

type instance XProg Normal = NoExt
type instance XLet Normal = NoExt
type instance XExternal Normal = NoExt
type instance XType Normal = NoExt
type instance XExternal Normal = NoExt

type instance XBTyped Normal = Bounds
type instance XBRaw Normal = NoExt

-- Deriving

deriving instance Show (Name Normal)
deriving instance Show (Type Normal)
deriving instance Show (Pattern Normal)
deriving instance Show (Literal Normal)
deriving instance Show (Expr Normal)
deriving instance Show (TypeCons Normal)
deriving instance Show (Binder Normal)
deriving instance Show (TypeDecl Normal)
deriving instance Show (Program Normal)
deriving instance Show (ExternalDecl Normal)
deriving instance Show (LetDecl Normal)
deriving instance Show (ImportDecl Normal)

-- Position 

class HasPosition a where 
    getPos :: a -> Bounds

instance (HasPosition a, HasPosition b) => HasPosition (a,b) where 
    getPos (a,b) = getPos a <> getPos b

instance HasPosition a => HasPosition [a] where 
    getPos [] = empty 
    getPos x = getPos (head x) <> getPos (last x)

instance HasPosition (Binder Normal) where 
    getPos (Raw _ ty) = getPos ty 
    getPos (Typed pos _ _) = pos

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
    getPos (PId _ n) = getPos n

instance HasPosition (Literal Normal) where 
    getPos (LChar pos _) = pos 
    getPos (LString pos _) = pos 
    getPos (LInt pos _) = pos 
    getPos (LDouble pos _) = pos 

instance HasPosition (Expr Normal) where 
    getPos (Lam pos _ _) = pos 
    getPos (App pos _ _) = pos 
    getPos (Var _ name) = getPos name 
    getPos (Lit _ lit) = getPos lit 
    getPos (Assign pos _ _) = pos 
    getPos (Match pos _ _) = pos 
    getPos (Binary pos _ _ _) = pos
    getPos (Block pos _) = pos 

instance HasPosition (TypeCons Normal) where 
    getPos (TcSum pos _) = pos 
    getPos (TcRecord pos _) = pos 
    getPos (TcSyn _ ty) = getPos ty 

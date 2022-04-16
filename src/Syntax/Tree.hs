{-| Here we create a lot of type instances for 
    a normal AST that stores Ranges inside of it.
-}
module Syntax.Tree (Normal, HasPosition(..)) where 
    
import Expr 
import Data.Void  (Void)
import Syntax.Range (Range, HasPosition (getPos))

import qualified Data.Text as Text

data Normal

type instance XName Normal = Range

type instance XTSimple Normal = NoExt
type instance XTPoly Normal = NoExt
type instance XTArrow Normal = Range
type instance XTApp Normal = Range
type instance XTForall Normal = Range
type instance XTExt Normal = Void

type instance XPWild Normal = Range
type instance XPCons Normal = Range
type instance XPLit Normal = NoExt
type instance XPId Normal = NoExt
type instance XPExt Normal = Void

type instance XLChar Normal = Range
type instance XLString Normal = Range
type instance XLInt Normal = Range
type instance XLDouble Normal = Range
type instance XLExt Normal = Void

type instance XLam Normal = Range
type instance XApp Normal = Range
type instance XVar Normal = NoExt
type instance XAnn Normal = Range
type instance XLit Normal = NoExt
type instance XMatch Normal = Range
type instance XAssign Normal =  Range
type instance XBinary Normal =  Range
type instance XBlock Normal = Range
type instance XIf Normal = Range
type instance XExt Normal = Void

type instance XTcSum Normal = Range
type instance XTcRecord Normal = Range
type instance XTcSyn Normal = NoExt
type instance XTcExt Normal = Void

type instance XProg Normal = NoExt
type instance XLet Normal = NoExt
type instance XExternal Normal = NoExt
type instance XType Normal = NoExt
type instance XExternal Normal = NoExt

type instance XBTyped Normal = Range
type instance XBRaw Normal = NoExt

-- Deriving

deriving instance Show (Name Normal)

instance Show (Typer Normal) where 
    show = \case
      TSimple _ (Name _ na) -> Text.unpack na
      TPoly _ (Name _ na) -> Text.unpack na
      TArrow _ ty ty' -> "(" ++ show ty ++ " -> " ++ show ty' ++ ")"
      TApp _ ty ty' -> "(" ++ show ty ++ " " ++ show ty' ++ ")"
      TForall _ (Name _ na) ty -> "(∀" ++ Text.unpack na ++ "." ++ show ty ++ ")"

deriving instance Show (Pattern Normal)
deriving instance Show (Literal Normal)
deriving instance Show (Expr Normal)
deriving instance Show (TypeCons Normal)
deriving instance Show (Sttms Normal)
deriving instance Show (Assign Normal)
deriving instance Show (Binder Normal)
deriving instance Show (TypeDecl Normal)
deriving instance Show (Program Normal)
deriving instance Show (ExternalDecl Normal)
deriving instance Show (LetDecl Normal)
deriving instance Show (ImportDecl Normal)

instance Eq (Name Normal) where 
    (Name _ n) == (Name _ m) = n == m

instance Ord (Name Normal) where 
   compare (Name _ n) (Name _ m) = compare n m

-- Position 

instance HasPosition (Binder Normal) where 
    getPos (Raw _ ty) = getPos ty 
    getPos (Typed pos _ _) = pos

instance HasPosition (Name Normal) where 
    getPos (Name pos _) = pos

instance HasPosition (Typer Normal) where 
    getPos (TSimple _ name) = getPos name
    getPos (TPoly _ name) = getPos name
    getPos (TArrow pos _ _) = pos 
    getPos (TApp pos _ _) = pos
    getPos (TForall pos _ _) = pos 
     
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
    getPos (Ann pos _ _) = pos 
    getPos (App pos _ _) = pos 
    getPos (Var _ name) = getPos name 
    getPos (Lit _ lit) = getPos lit 
    getPos (Match pos _ _) = pos 
    getPos (Binary pos _ _ _) = pos
    getPos (Block pos _) = pos 
    getPos (If pos _ _ _) = pos 
    
instance HasPosition (TypeCons Normal) where 
    getPos (TcSum pos _) = pos 
    getPos (TcRecord pos _) = pos 
    getPos (TcSyn _ ty) = getPos ty 

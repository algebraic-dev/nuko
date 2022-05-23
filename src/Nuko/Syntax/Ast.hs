module Nuko.Syntax.Ast (Normal) where

import Nuko.Tree.Expr
import Nuko.Tree.TopLevel
import Data.Void (Void, absurd)
import Nuko.Syntax.Range ( Range, HasPosition(..) )

data Normal

type instance XName Normal = Range
type instance XLInt Normal = Range
type instance XLStr Normal = Range

type instance XTId Normal = NoExt
type instance XTPoly Normal = NoExt
type instance XTCons Normal = Range
type instance XTArrow Normal = Range
type instance XTForall Normal = Range

type instance XPWild Normal = Range
type instance XPId Normal = NoExt
type instance XPLit Normal = NoExt
type instance XPAnn Normal = Range
type instance XPCons Normal = Range
type instance XPExt Normal = Void

type instance XLit Normal = NoExt
type instance XLam Normal = Range
type instance XAnn Normal = Range
type instance XApp Normal = Range
type instance XLower Normal = Range
type instance XUpper Normal = Range
type instance XAccessor Normal = Range
type instance XIf Normal = Range
type instance XCase Normal = Range
type instance XBlock Normal = Range
type instance XVar Normal = Range
type instance XExt Normal = Void

type instance XPath Normal = NoExt
type instance XNaExt Normal = Void
type instance XPaExt Normal = Void

type instance XLetDecl Normal = NoExt
type instance XProgram Normal = NoExt
type instance XTypeDecl Normal = NoExt

type instance XTypeSym Normal = NoExt
type instance XTypeProd Normal = NoExt
type instance XTypeSum Normal = NoExt
type instance XImport Normal = NoExt

deriving instance Show (Name Normal)
deriving instance Show (Expr Normal)
deriving instance Show (Block Normal)
deriving instance Show (Var Normal)
deriving instance Show (Literal Normal)
deriving instance Show (Pat Normal)
deriving instance Show (Type Normal)
deriving instance Show (Path Normal)
deriving instance Show (Import Normal)
deriving instance Show (Program Normal)
deriving instance Show (TypeDeclArg Normal)
deriving instance Show (TypeDecl Normal)
deriving instance Show (LetDecl Normal)

instance HasPosition (Name Normal) where
  getPos (Name _ r) = r
  getPos (NaExt r)  = absurd r

instance HasPosition (Var Normal) where
  getPos (Var _ _ r) = r

instance HasPosition (Path Normal) where
  getPos (Path [] f _ ) = getPos f
  getPos (Path (x : _) f _) = getPos x <> getPos f
  getPos (PaExt r) = absurd r

instance HasPosition (Literal Normal) where
  getPos = \case
    LStr _ r -> r
    LInt _ r -> r

instance HasPosition (Pat Normal) where
  getPos = \case
    PWild r -> r
    PId id' _ -> getPos id'
    PCons _ _ r -> r
    PLit i _ -> getPos i
    PExt r -> absurd r
    PAnn _ _ r -> r

instance HasPosition (Expr Normal) where
  getPos = \case
    Lit t _ -> getPos t
    Lam _ _ r -> r
    App _ _ r -> r
    Lower _ r -> r
    Upper _ r -> r
    Accessor _ _ r -> r
    If _ _ _ r -> r
    Case _ _ r -> r
    Block _  r -> r
    Ann _ _ r  -> r

instance HasPosition (Block Normal) where
  getPos = \case
    BlBind x r           -> getPos x <> getPos r
    BlVar (Var _ _ r1) r -> r1 <> getPos r
    BlEnd x              -> getPos x

instance HasPosition (Type Normal) where
  getPos = \case
    TId n _       -> getPos n
    TPoly n _     -> getPos n
    TCons _ _ r   -> r
    TArrow _ _ r  -> r
    TForall _ _ r -> r
module Nuko.Resolver.Resolved (
  Resolved,
  ResPath(..)
) where

import Nuko.Tree.Expr
import Nuko.Tree.TopLevel
import Data.Void          (Void, absurd)
import Nuko.Syntax.Range  (Range, HasPosition(..))

data Resolved

data ResPath = ResPath (Name Resolved) (Name Resolved) (Range)

type instance XLInt Resolved = Range
type instance XLStr Resolved = Range

type instance XTId Resolved = NoExt
type instance XTPoly Resolved = NoExt
type instance XTCons Resolved = Range
type instance XTArrow Resolved = Range
type instance XTForall Resolved = Range

type instance XPWild Resolved = Range
type instance XPId Resolved = NoExt
type instance XPLit Resolved = NoExt
type instance XPAnn Resolved = Range
type instance XPCons Resolved = Range
type instance XPExt Resolved = Void

type instance XLit Resolved = NoExt
type instance XLam Resolved = Range
type instance XAnn Resolved = Range
type instance XApp Resolved = Range
type instance XLower Resolved = Range
type instance XUpper Resolved = Range
type instance XAccessor Resolved = Range
type instance XIf Resolved = Range
type instance XCase Resolved = Range
type instance XBlock Resolved = Range
type instance XVar Resolved = Range
type instance XExt Resolved = Void

type instance XPath Resolved  = Range
type instance XName Resolved  = Range
type instance XNaExt Resolved = Void
type instance XPaExt Resolved = ResPath

type instance XLetDecl Resolved  = NoExt
type instance XProgram Resolved  = NoExt
type instance XTypeDecl Resolved = NoExt
type instance XImport Resolved   = NoExt

type instance XTypeSym Resolved  = NoExt
type instance XTypeProd Resolved = NoExt
type instance XTypeSum Resolved  = NoExt

deriving instance Show (ResPath)
deriving instance Show (Name Resolved)
deriving instance Show (Expr Resolved)
deriving instance Show (Block Resolved)
deriving instance Show (Var Resolved)
deriving instance Show (Literal Resolved)
deriving instance Show (Pat Resolved)
deriving instance Show (Type Resolved)
deriving instance Show (Path Resolved)
deriving instance Show (Import Resolved)
deriving instance Show (Program Resolved)
deriving instance Show (TypeDeclArg Resolved)
deriving instance Show (TypeDecl Resolved)
deriving instance Show (LetDecl Resolved)

instance HasPosition (Name Resolved) where
  getPos (Name _ r) = r
  getPos (NaExt r) = absurd r

instance HasPosition (Var Resolved) where
  getPos (Var _ _ r) = r

instance HasPosition (Path Resolved) where
  getPos (Path  _ _ r)           = r
  getPos (PaExt (ResPath _ _ r)) = r

instance HasPosition (Literal Resolved) where
  getPos = \case
    LStr _ r -> r
    LInt _ r -> r

instance HasPosition (Pat Resolved) where
  getPos = \case
    PWild r -> r
    PId id' _ -> getPos id'
    PCons _ _ r -> r
    PLit i _ -> getPos i
    PExt r -> absurd r
    PAnn _ _ r -> r

instance HasPosition (Expr Resolved) where
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

instance HasPosition (Block Resolved) where
  getPos = \case
    BlBind x r  -> getPos x <> getPos r
    BlVar (Var _ _ r1) r -> r1 <> getPos r
    BlEnd x  -> getPos x

instance HasPosition (Type Resolved) where
  getPos = \case
    TId n _ -> getPos n
    TPoly n _ -> getPos n
    TCons _ _ r -> r
    TArrow _ _ r -> r
    TForall _ _ r -> r
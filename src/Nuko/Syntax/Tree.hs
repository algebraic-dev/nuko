{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This modules instantiates the tree to Nuko 'Normal
--   Yeah.. i use a lot of orphan instances here :P
module Nuko.Syntax.Tree where

import Relude

import Nuko.Names        (Ident, ModName, Name, Path)
import Nuko.Report.Range (HasPosition (..), Range)
import Nuko.Tree
import Pretty.Tree       (PrettyTree)

type Normal = Nuko 'Normal

type instance XIdent Nm  = Ident
type instance XModName Nm  = ModName
type instance XName Nm k = Name k
type instance XPath Nm k = Path (Name k)
type instance XTy   Nm = Ty Nm

type instance XLInt Nm = Range
type instance XLStr Nm = Range

type instance XTId Nm = NoExt
type instance XTPoly Nm = NoExt
type instance XTCons Nm = Range
type instance XTArrow Nm = Range
type instance XTForall Nm = Range

type instance XPWild Nm = Range
type instance XPId Nm = NoExt
type instance XPLit Nm = NoExt
type instance XPAnn Nm = Range
type instance XPOr Nm = Range
type instance XPCons Nm = Range
type instance XPRec Nm = Range
type instance XPExt Nm = Void

type instance XRecMono Nm = Range
type instance XRecCreate Nm = Range
type instance XRecBinder Nm = Range

type instance XLit Nm = NoExt
type instance XLam Nm = Range
type instance XAnn Nm = Range
type instance XApp Nm = Range
type instance XLower Nm = NoExt
type instance XUpper Nm = NoExt
type instance XField Nm = Range
type instance XIf Nm = Range
type instance XRecUpdate Nm = Range
type instance XRecCreate Nm = Range
type instance XBinOp Nm = Range
type instance XMatch Nm = Range
type instance XBlock Nm = Range
type instance XVar Nm = Range
type instance XExt Nm = Void

type instance XImport Nm = NoExt

deriving instance Generic (Expr Nm)
deriving instance Generic (Block Nm)
deriving instance Generic (Var Nm)
deriving instance Generic (Literal Nm)
deriving instance Generic (Pat Nm)
deriving instance Generic (Ty Nm)
deriving instance Generic (Import Nm)
deriving instance Generic (ImportDepsKind Nm)
deriving instance Generic (ImportDeps Nm)
deriving instance Generic (ImportModifier Nm)
deriving instance Generic (Program Nm)
deriving instance Generic (TypeDeclArg Nm)
deriving instance Generic (TypeDecl Nm)
deriving instance Generic (LetDecl Nm)
deriving instance Generic (RecordBinder Pat Nm)
deriving instance Generic (RecordBinder Expr Nm)

instance PrettyTree (RecordBinder Pat Nm) where
instance PrettyTree (RecordBinder Expr Nm) where
instance PrettyTree (Expr Nm) where
instance PrettyTree (Block Nm) where
instance PrettyTree (Var Nm) where
instance PrettyTree (Literal Nm) where
instance PrettyTree (Pat Nm) where
instance PrettyTree (Ty Nm) where
instance PrettyTree (Import Nm) where
instance PrettyTree (ImportDepsKind Nm) where
instance PrettyTree (ImportDeps Nm) where
instance PrettyTree (ImportModifier Nm) where
instance PrettyTree (Program Nm) where
instance PrettyTree (TypeDeclArg Nm) where
instance PrettyTree (TypeDecl Nm) where
instance PrettyTree (LetDecl Nm) where

instance HasPosition (Var Nm) where
  getPos (Var _ _ r) = r

instance HasPosition (Literal Nm) where
  getPos = \case
    LStr _ r -> r
    LInt _ r -> r

instance HasPosition (Pat Nm) where
  getPos = \case
    PWild r     -> r
    PCons _ _ r -> r
    PLit i _    -> getPos i
    PAnn _ _ r  -> r
    PId n _     -> getPos n
    POr _ _ r   -> r
    PRec _ _ r  -> r

instance HasPosition (Expr Nm) where
  getPos = \case
    Lit t _         -> getPos t
    Lam _ _ r       -> r
    App _ _ r       -> r
    Lower n _       -> getPos n
    Upper n _       -> getPos n
    Field _ _ r     -> r
    If _ _ _ r      -> r
    Match _ _ r     -> r
    Block _  r      -> r
    Ann _ _ r       -> r
    RecUpdate _ _ r -> r
    RecCreate _ _ r -> r
    BinOp _ _ _ r   -> r

instance HasPosition (Block Nm) where
  getPos = \case
    BlBind x r           -> getPos x <> getPos r
    BlVar (Var _ _ r1) r -> r1 <> getPos r
    BlEnd x              -> getPos x

instance HasPosition (Ty Nm) where
  getPos = \case
    TId n _       -> getPos n
    TPoly n _     -> getPos n
    TApp _ _ r    -> r
    TArrow _ _ r  -> r
    TForall _ _ r -> r

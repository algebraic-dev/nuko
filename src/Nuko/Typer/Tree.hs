{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Nuko.Typer.Tree where

import Relude

import Nuko.Names        (Ident, ModName, Name, Path)
import Nuko.Report.Range (HasPosition (..), Range)
import Nuko.Tree
import Nuko.Typer.Types  (Relation (..), TTy)
import Pretty.Tree       (PrettyTree)

type instance XIdent Tc    = Ident
type instance XModName Tc  = ModName
type instance XName Tc k   = Name k
type instance XPath Tc k   = Path (Name k)
type instance XTy Tc       = TTy 'Real

type instance XLInt Tc = (XTy Tc, Range)
type instance XLStr Tc = (XTy Tc, Range)

type instance XPWild Tc = (XTy Tc, Range)
type instance XPId Tc = XTy Tc
type instance XPId Tc = XTy Tc
type instance XPOr Tc = (XTy Tc, Range)
type instance XPLit Tc = NoExt
type instance XPAnn Tc = (XTy Tc, Range)
type instance XPCons Tc = (XTy Tc, Range)
type instance XPExt Tc = Void

type instance XLit Tc = NoExt
type instance XLam Tc = (XTy Tc, Range)
type instance XAnn Tc = Range
type instance XApp Tc = (XTy Tc, Range)
type instance XLower Tc = XTy Tc
type instance XUpper Tc = XTy Tc
type instance XField Tc = (XTy Tc, Range)
type instance XIf Tc = (XTy Tc, Range)
type instance XMatch Tc = (XTy Tc, Range)
type instance XBlock Tc = (XTy Tc, Range)
type instance XVar Tc = Range
type instance XExt Tc = Void

type instance XImport Tc = Void

deriving instance Generic (Expr Tc)
deriving instance Generic (Block Tc)
deriving instance Generic (Var Tc)
deriving instance Generic (Literal Tc)
deriving instance Generic (Pat Tc)
deriving instance Generic (Ty Tc)
deriving instance Generic (Import Tc)
deriving instance Generic (ImportDepsKind Tc)
deriving instance Generic (ImportDeps Tc)
deriving instance Generic (ImportModifier Tc)
deriving instance Generic (Program Tc)
deriving instance Generic (TypeDeclArg Tc)
deriving instance Generic (TypeDecl Tc)
deriving instance Generic (LetDecl Tc)

instance PrettyTree (Expr Tc) where
instance PrettyTree (Block Tc) where
instance PrettyTree (Var Tc) where
instance PrettyTree (Literal Tc) where
instance PrettyTree (Pat Tc) where
instance PrettyTree (Import Tc) where
instance PrettyTree (ImportDepsKind Tc) where
instance PrettyTree (ImportDeps Tc) where
instance PrettyTree (ImportModifier Tc) where
instance PrettyTree (Program Tc) where
instance PrettyTree (TypeDeclArg Tc) where
instance PrettyTree (TypeDecl Tc) where
instance PrettyTree (LetDecl Tc) where

instance HasPosition (Var Tc) where
  getPos (Var _ _ r) = r

instance HasPosition (Literal Tc) where
  getPos = \case
    LStr _ (_, r) -> r
    LInt _ (_, r) -> r

instance HasPosition (Pat Tc) where
  getPos = \case
    PWild (_, r)     -> r
    PCons _ _ (_, r) -> r
    PLit i _         -> getPos i
    POr _ _ r        -> snd r
    PAnn _ _ r       -> snd r
    PId n _          -> getPos n

instance HasPosition (Expr Tc) where
  getPos = \case
    Lit t _          -> getPos t
    Lam _ _ (_, r)   -> r
    App _ _ (_, r)   -> r
    Lower r _        -> getPos r
    Upper r _        -> getPos r
    Field _ _ (_, r) -> r
    If _ _ _ (_, r)  -> r
    Match _ _ (_, r) -> r
    Block _  (_, r)  -> r
    Ann _ _ r        -> r

instance HasPosition (Block Tc) where
  getPos = \case
    BlBind x r           -> getPos x <> getPos r
    BlVar (Var _ _ r1) r -> r1 <> getPos r
    BlEnd x              -> getPos x

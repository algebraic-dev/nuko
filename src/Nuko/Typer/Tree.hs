{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Nuko.Typer.Tree where

import Nuko.Tree
import Nuko.Syntax.Range  (Range, HasPosition(..))
import Nuko.Resolver.Tree (ReId, Path)
import Pretty.Tree        (PrettyTree)
import Relude             (Semigroup((<>)), Void, Generic)

type instance XName Tc = ReId
type instance XPath Tc = Path

type instance XLInt Tc = Range
type instance XLStr Tc = Range

type instance XTId Tc = NoExt
type instance XTPoly Tc = NoExt
type instance XTCons Tc = Range
type instance XTArrow Tc = Range
type instance XTForall Tc = Range

type instance XPWild Tc = Range
type instance XPId Tc = NoExt
type instance XPLit Tc = NoExt
type instance XPAnn Tc = Range
type instance XPCons Tc = Range
type instance XPExt Tc = Void

type instance XLit Tc = NoExt
type instance XLam Tc = Range
type instance XAnn Tc = Range
type instance XApp Tc = Range
type instance XLower Tc = Range
type instance XUpper Tc = Range
type instance XField Tc = Range
type instance XIf Tc = Range
type instance XMatch Tc = Range
type instance XBlock Tc = Range
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
instance PrettyTree (Ty Tc) where
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
    LStr _ r -> r
    LInt _ r -> r

instance HasPosition (Pat Tc) where
  getPos = \case
    PWild r     -> r
    PCons _ _ r -> r
    PLit i _    -> getPos i
    PAnn _ _ r  -> r
    PId n _     -> getPos n

instance HasPosition (Expr Tc) where
  getPos = \case
    Lit t _ -> getPos t
    Lam _ _ r -> r
    App _ _ r -> r
    Lower _ r -> r
    Upper _ r -> r
    Field _ _ r -> r
    If _ _ _ r -> r
    Match _ _ r -> r
    Block _  r -> r
    Ann _ _ r  -> r

instance HasPosition (Block Tc) where
  getPos = \case
    BlBind x r           -> getPos x <> getPos r
    BlVar (Var _ _ r1) r -> r1 <> getPos r
    BlEnd x              -> getPos x

instance HasPosition (Ty Tc) where
  getPos = \case
    TId n _       -> getPos n
    TPoly n _     -> getPos n
    TApp _ _ r    -> r
    TArrow _ _ r  -> r
    TForall _ _ r -> r
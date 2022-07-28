{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This modules instantiates the tree but changed
--   only a few things like Names and Paths that are
--   really important for the resolver

module Nuko.Resolver.Tree where

import Nuko.Tree
import Nuko.Report.Range  (Range, HasPosition(..))
import Relude             (Semigroup((<>)), Void, Generic)
import Pretty.Tree        (PrettyTree)
import Nuko.Names         (Path, Name, Ident, ModName)

type instance XIdent Re    = Ident
type instance XModName Re  = ModName
type instance XName Re k   = Name k
type instance XPath Re k   = Path (Name k)
type instance XTy Re       = Ty Re

type instance XLInt Re = Range
type instance XLStr Re = Range

type instance XTId Re = NoExt
type instance XTPoly Re = NoExt
type instance XTCons Re = Range
type instance XTArrow Re = Range
type instance XTForall Re = Range

type instance XPWild Re = Range
type instance XPId Re = NoExt
type instance XPLit Re = NoExt
type instance XPAnn Re = Range
type instance XPOr Re = Range
type instance XPCons Re = Range
type instance XPExt Re = Void

type instance XLit Re = NoExt
type instance XLam Re = Range
type instance XAnn Re = Range
type instance XApp Re = Range
type instance XLower Re = NoExt
type instance XUpper Re = NoExt
type instance XField Re = Range
type instance XIf Re = Range
type instance XMatch Re = Range
type instance XBlock Re = Range
type instance XVar Re = Range
type instance XExt Re = Void

type instance XImport Re = Void

deriving instance Generic (Expr Re)
deriving instance Generic (Block Re)
deriving instance Generic (Var Re)
deriving instance Generic (Literal Re)
deriving instance Generic (Pat Re)
deriving instance Generic (Ty Re)
deriving instance Generic (Import Re)
deriving instance Generic (ImportDepsKind Re)
deriving instance Generic (ImportDeps Re)
deriving instance Generic (ImportModifier Re)
deriving instance Generic (Program Re)
deriving instance Generic (TypeDeclArg Re)
deriving instance Generic (TypeDecl Re)
deriving instance Generic (LetDecl Re)

instance PrettyTree (Expr Re) where
instance PrettyTree (Block Re) where
instance PrettyTree (Var Re) where
instance PrettyTree (Literal Re) where
instance PrettyTree (Pat Re) where
instance PrettyTree (Ty Re) where
instance PrettyTree (Import Re) where
instance PrettyTree (ImportDepsKind Re) where
instance PrettyTree (ImportDeps Re) where
instance PrettyTree (ImportModifier Re) where
instance PrettyTree (Program Re) where
instance PrettyTree (TypeDeclArg Re) where
instance PrettyTree (TypeDecl Re) where
instance PrettyTree (LetDecl Re) where

instance HasPosition (Var Re) where
  getPos (Var _ _ r) = r

instance HasPosition (Literal Re) where
  getPos = \case
    LStr _ r -> r
    LInt _ r -> r

instance HasPosition (Pat Re) where
  getPos = \case
    PWild r     -> r
    PCons _ _ r -> r
    PLit i _    -> getPos i
    PAnn _ _ r  -> r
    PId n _     -> getPos n
    POr _ _ r   -> r

instance HasPosition (Expr Re) where
  getPos = \case
    Lit t _ -> getPos t
    Lam _ _ r -> r
    App _ _ r -> r
    Lower r _ -> getPos r
    Upper r _ -> getPos r
    Field _ _ r -> r
    If _ _ _ r -> r
    Match _ _ r -> r
    Block _  r -> r
    Ann _ _ r  -> r

instance HasPosition (Block Re) where
  getPos = \case
    BlBind x r           -> getPos x <> getPos r
    BlVar (Var _ _ r1) r -> r1 <> getPos r
    BlEnd x              -> getPos x

instance HasPosition (Ty Re) where
  getPos = \case
    TId n _       -> getPos n
    TPoly n _     -> getPos n
    TApp  _ _ r   -> r
    TArrow _ _ r  -> r
    TForall _ _ r -> r
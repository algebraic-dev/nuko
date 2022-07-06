{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This modules instantiates the tree but changed
--   only a few things like Names and Paths that are
--   really important for the resolver

module Nuko.Resolver.Tree where

import Nuko.Tree
import Nuko.Syntax.Range  (Range, HasPosition(..), toLabel)
import Relude             (Show, Semigroup((<>)), Void, Generic, show)
import Data.Text          (Text)
import Pretty.Tree        (PrettyTree (prettyTree), Tree (Node))

data ReId = ReId { text :: Text, range :: Range } deriving (Show, Generic)

data Path
  = Path Text ReId Range
  | Local ReId
  deriving Show

type instance XName (Nuko 'Resolved) = ReId
type instance XPath (Nuko 'Resolved) = Path

type instance XLInt (Nuko 'Resolved) = Range
type instance XLStr (Nuko 'Resolved) = Range

type instance XTId (Nuko 'Resolved) = NoExt
type instance XTPoly (Nuko 'Resolved) = NoExt
type instance XTCons (Nuko 'Resolved) = Range
type instance XTArrow (Nuko 'Resolved) = Range
type instance XTForall (Nuko 'Resolved) = Range

type instance XPWild (Nuko 'Resolved) = Range
type instance XPId (Nuko 'Resolved) = NoExt
type instance XPLit (Nuko 'Resolved) = NoExt
type instance XPAnn (Nuko 'Resolved) = Range
type instance XPCons (Nuko 'Resolved) = Range
type instance XPExt (Nuko 'Resolved) = Void

type instance XLit (Nuko 'Resolved) = NoExt
type instance XLam (Nuko 'Resolved) = Range
type instance XAnn (Nuko 'Resolved) = Range
type instance XApp (Nuko 'Resolved) = Range
type instance XLower (Nuko 'Resolved) = Range
type instance XUpper (Nuko 'Resolved) = Range
type instance XField (Nuko 'Resolved) = Range
type instance XIf (Nuko 'Resolved) = Range
type instance XMatch (Nuko 'Resolved) = Range
type instance XBlock (Nuko 'Resolved) = Range
type instance XVar (Nuko 'Resolved) = Range
type instance XExt (Nuko 'Resolved) = Void

type instance XImport (Nuko 'Resolved) = Void

deriving instance Generic (Expr (Nuko 'Resolved))
deriving instance Generic (Block (Nuko 'Resolved))
deriving instance Generic (Var (Nuko 'Resolved))
deriving instance Generic (Literal (Nuko 'Resolved))
deriving instance Generic (Pat (Nuko 'Resolved))
deriving instance Generic (Ty (Nuko 'Resolved))
deriving instance Generic (Import (Nuko 'Resolved))
deriving instance Generic (ImportDepsKind (Nuko 'Resolved))
deriving instance Generic (ImportDeps (Nuko 'Resolved))
deriving instance Generic (ImportModifier (Nuko 'Resolved))
deriving instance Generic (Program (Nuko 'Resolved))
deriving instance Generic (TypeDeclArg (Nuko 'Resolved))
deriving instance Generic (TypeDecl (Nuko 'Resolved))
deriving instance Generic (LetDecl (Nuko 'Resolved))

instance PrettyTree Path  where
  prettyTree (Path mod' t r) = Node "Path" [show (mod' <> "." <> t.text), toLabel r] []
  prettyTree (Local t) = Node "Local" [show t.text, toLabel t.range] []

instance PrettyTree ReId  where prettyTree a = Node "ReId" [a.text, toLabel a.range] []

instance PrettyTree (Expr (Nuko 'Resolved)) where
instance PrettyTree (Block (Nuko 'Resolved)) where
instance PrettyTree (Var (Nuko 'Resolved)) where
instance PrettyTree (Literal (Nuko 'Resolved)) where
instance PrettyTree (Pat (Nuko 'Resolved)) where
instance PrettyTree (Ty (Nuko 'Resolved)) where
instance PrettyTree (Import (Nuko 'Resolved)) where
instance PrettyTree (ImportDepsKind (Nuko 'Resolved)) where
instance PrettyTree (ImportDeps (Nuko 'Resolved)) where
instance PrettyTree (ImportModifier (Nuko 'Resolved)) where
instance PrettyTree (Program (Nuko 'Resolved)) where
instance PrettyTree (TypeDeclArg (Nuko 'Resolved)) where
instance PrettyTree (TypeDecl (Nuko 'Resolved)) where
instance PrettyTree (LetDecl (Nuko 'Resolved)) where

instance HasPosition ReId where
  getPos (ReId _ r) = r

instance HasPosition Path where
  getPos (Path _ _ r) = r
  getPos (Local r) = r.range

instance HasPosition (Var (Nuko 'Resolved)) where
  getPos (Var _ _ r) = r

instance HasPosition (Literal (Nuko 'Resolved)) where
  getPos = \case
    LStr _ r -> r
    LInt _ r -> r

instance HasPosition (Pat (Nuko 'Resolved)) where
  getPos = \case
    PWild r     -> r
    PCons _ _ r -> r
    PLit i _    -> getPos i
    PAnn _ _ r  -> r
    PId n _     -> getPos n

instance HasPosition (Expr (Nuko 'Resolved)) where
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

instance HasPosition (Block (Nuko 'Resolved)) where
  getPos = \case
    BlBind x r           -> getPos x <> getPos r
    BlVar (Var _ _ r1) r -> r1 <> getPos r
    BlEnd x              -> getPos x

instance HasPosition (Ty (Nuko 'Resolved)) where
  getPos = \case
    TId n _       -> getPos n
    TPoly n _     -> getPos n
    TApp  _ _ r   -> r
    TArrow _ _ r  -> r
    TForall _ _ r -> r
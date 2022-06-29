{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This modules instantiates the tree but changed
--   only a few things like Names and Paths that are
--   really important for the resolver
module Nuko.Resolver.Tree where

import Relude             (Show, Semigroup((<>)), Void)
import Data.Text          (Text)
import Nuko.Tree
import Nuko.Syntax.Range  (Range, HasPosition(..))

data Name = Name { text :: Text, range :: Range } deriving Show
data Path = Path { mod :: Name, last :: Name, range :: Range } deriving Show

type Renamed = Nuko 'Renamed

type instance XName (Nuko 'Renamed) = Name
type instance XPath (Nuko 'Renamed) = Path

type instance XLInt (Nuko 'Renamed) = Range
type instance XLStr (Nuko 'Renamed) = Range

type instance XTId (Nuko 'Renamed) = NoExt
type instance XTPoly (Nuko 'Renamed) = NoExt
type instance XTCons (Nuko 'Renamed) = Range
type instance XTArrow (Nuko 'Renamed) = Range
type instance XTForall (Nuko 'Renamed) = Range

type instance XPWild (Nuko 'Renamed) = Range
type instance XPId (Nuko 'Renamed) = NoExt
type instance XPLit (Nuko 'Renamed) = NoExt
type instance XPAnn (Nuko 'Renamed) = Range
type instance XPCons (Nuko 'Renamed) = Range
type instance XPExt (Nuko 'Renamed) = Void

type instance XLit (Nuko 'Renamed) = NoExt
type instance XLam (Nuko 'Renamed) = Range
type instance XAnn (Nuko 'Renamed) = Range
type instance XApp (Nuko 'Renamed) = Range
type instance XLower (Nuko 'Renamed) = Range
type instance XUpper (Nuko 'Renamed) = Range
type instance XField (Nuko 'Renamed) = Range
type instance XIf (Nuko 'Renamed) = Range
type instance XMatch (Nuko 'Renamed) = Range
type instance XBlock (Nuko 'Renamed) = Range
type instance XVar (Nuko 'Renamed) = Range
type instance XExt (Nuko 'Renamed) = Void

deriving instance Show (Expr (Nuko 'Renamed))
deriving instance Show (Block (Nuko 'Renamed))
deriving instance Show (Var (Nuko 'Renamed))
deriving instance Show (Literal (Nuko 'Renamed))
deriving instance Show (Pat (Nuko 'Renamed))
deriving instance Show (Ty (Nuko 'Renamed))
deriving instance Show (Import (Nuko 'Renamed))
deriving instance Show (ImportDepsKind (Nuko 'Renamed))
deriving instance Show (ImportDeps (Nuko 'Renamed))
deriving instance Show (ImportTree (Nuko 'Renamed))
deriving instance Show (Program (Nuko 'Renamed))
deriving instance Show (TypeDeclArg (Nuko 'Renamed))
deriving instance Show (TypeDecl (Nuko 'Renamed))
deriving instance Show (LetDecl (Nuko 'Renamed))

instance HasPosition Path where
  getPos (Path _ _ r) = r

instance HasPosition Name where
  getPos (Name _ r) = r

instance HasPosition (Var (Nuko 'Renamed)) where
  getPos (Var _ _ r) = r

instance HasPosition (Literal (Nuko 'Renamed)) where
  getPos = \case
    LStr _ r -> r
    LInt _ r -> r

instance HasPosition (Pat (Nuko 'Renamed)) where
  getPos = \case
    PWild r     -> r
    PCons _ _ r -> r
    PLit i _    -> getPos i
    PAnn _ _ r  -> r
    PId n _     -> getPos n

instance HasPosition (Expr (Nuko 'Renamed)) where
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

instance HasPosition (Block (Nuko 'Renamed)) where
  getPos = \case
    BlBind x r           -> getPos x <> getPos r
    BlVar (Var _ _ r1) r -> r1 <> getPos r
    BlEnd x              -> getPos x

instance HasPosition (Ty (Nuko 'Renamed)) where
  getPos = \case
    TId n _       -> getPos n
    TPoly n _     -> getPos n
    TCons _ _ r   -> r
    TArrow _ _ r  -> r
    TForall _ _ r -> r
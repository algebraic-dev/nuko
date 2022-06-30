{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This modules instantiates the tree to Nuko 'Normal
--   Yeah.. i use a lot of orphan instances here :P
module Nuko.Syntax.Tree where

import Relude             ( Show, Semigroup((<>)), Void )
import Data.Text          ( Text )
import Nuko.Tree
import Nuko.Syntax.Range  ( Range, HasPosition(..) )

type Normal = Nuko 'Normal

data Name = Name { text :: Text, range :: Range } deriving Show
data Path = Path { mod :: [Name], last :: Name, range :: Range } deriving Show

type instance XName (Nuko 'Normal) = Name
type instance XPath (Nuko 'Normal) = Path

type instance XLInt (Nuko 'Normal) = Range
type instance XLStr (Nuko 'Normal) = Range

type instance XTId (Nuko 'Normal) = NoExt
type instance XTPoly (Nuko 'Normal) = NoExt
type instance XTCons (Nuko 'Normal) = Range
type instance XTArrow (Nuko 'Normal) = Range
type instance XTForall (Nuko 'Normal) = Range

type instance XPWild (Nuko 'Normal) = Range
type instance XPId (Nuko 'Normal) = NoExt
type instance XPLit (Nuko 'Normal) = NoExt
type instance XPAnn (Nuko 'Normal) = Range
type instance XPCons (Nuko 'Normal) = Range
type instance XPExt (Nuko 'Normal) = Void

type instance XLit (Nuko 'Normal) = NoExt
type instance XLam (Nuko 'Normal) = Range
type instance XAnn (Nuko 'Normal) = Range
type instance XApp (Nuko 'Normal) = Range
type instance XLower (Nuko 'Normal) = Range
type instance XUpper (Nuko 'Normal) = Range
type instance XField (Nuko 'Normal) = Range
type instance XIf (Nuko 'Normal) = Range
type instance XMatch (Nuko 'Normal) = Range
type instance XBlock (Nuko 'Normal) = Range
type instance XVar (Nuko 'Normal) = Range
type instance XExt (Nuko 'Normal) = Void

type instance XImport (Nuko 'Normal) = NoExt

deriving instance Show (Expr (Nuko 'Normal))
deriving instance Show (Block (Nuko 'Normal))
deriving instance Show (Var (Nuko 'Normal))
deriving instance Show (Literal (Nuko 'Normal))
deriving instance Show (Pat (Nuko 'Normal))
deriving instance Show (Ty (Nuko 'Normal))
deriving instance Show (Import (Nuko 'Normal))
deriving instance Show (ImportDepsKind (Nuko 'Normal))
deriving instance Show (ImportDeps (Nuko 'Normal))
deriving instance Show (ImportTree (Nuko 'Normal))
deriving instance Show (Program (Nuko 'Normal))
deriving instance Show (TypeDeclArg (Nuko 'Normal))
deriving instance Show (TypeDecl (Nuko 'Normal))
deriving instance Show (LetDecl (Nuko 'Normal))

instance HasPosition Path where
  getPos (Path _ _ r) = r

instance HasPosition Name where
  getPos (Name _ r) = r

instance HasPosition (Var (Nuko 'Normal)) where
  getPos (Var _ _ r) = r

instance HasPosition (Literal (Nuko 'Normal)) where
  getPos = \case
    LStr _ r -> r
    LInt _ r -> r

instance HasPosition (Pat (Nuko 'Normal)) where
  getPos = \case
    PWild r     -> r
    PCons _ _ r -> r
    PLit i _    -> getPos i
    PAnn _ _ r  -> r
    PId n _     -> getPos n

instance HasPosition (Expr (Nuko 'Normal)) where
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

instance HasPosition (Block (Nuko 'Normal)) where
  getPos = \case
    BlBind x r           -> getPos x <> getPos r
    BlVar (Var _ _ r1) r -> r1 <> getPos r
    BlEnd x              -> getPos x

instance HasPosition (Ty (Nuko 'Normal)) where
  getPos = \case
    TId n _       -> getPos n
    TPoly n _     -> getPos n
    TCons _ _ r   -> r
    TArrow _ _ r  -> r
    TForall _ _ r -> r
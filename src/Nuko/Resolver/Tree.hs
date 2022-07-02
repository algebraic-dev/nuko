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

data ReId = ReId { text :: Text, range :: Range } deriving Show

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

deriving instance Show (Expr (Nuko 'Resolved))
deriving instance Show (Block (Nuko 'Resolved))
deriving instance Show (Var (Nuko 'Resolved))
deriving instance Show (Literal (Nuko 'Resolved))
deriving instance Show (Pat (Nuko 'Resolved))
deriving instance Show (Ty (Nuko 'Resolved))
deriving instance Show (Import (Nuko 'Resolved))
deriving instance Show (ImportDepsKind (Nuko 'Resolved))
deriving instance Show (ImportDeps (Nuko 'Resolved))
deriving instance Show (ImportModifier (Nuko 'Resolved))
deriving instance Show (Program (Nuko 'Resolved))
deriving instance Show (TypeDeclArg (Nuko 'Resolved))
deriving instance Show (TypeDecl (Nuko 'Resolved))
deriving instance Show (LetDecl (Nuko 'Resolved))

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
    TCons _ _ r   -> r
    TArrow _ _ r  -> r
    TForall _ _ r -> r
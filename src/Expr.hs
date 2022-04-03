{-| Base module for every Syntax tree in "Trees that grow" format that
    will be used in the project. Probably this project will have only
    a few types of abstract trees
  
    - Normal: Only positions are stored inside the tree
    - Typed: Positions and types together
    - Lowered: No types or patterns are included here, it's an untyped ast
    - Optimized: No lambdas or things like that, it will have a few structures
-}
module Expr
  ( NoExt(..),
    Name (..),
    Binder (..),
    TypeCons (..),
    TypeDecl (..),
    Assign (..),
    LetDecl (..),
    ExternalDecl (..),
    ImportDecl (..),
    Program (..),
    TLKind (..),
    Pattern (..),
    Literal (..),
    Expr (..),
    Sttms (..),
    Typer (..),
    XName,
    XBTyped,
    XBRaw,
    XTForall,
    XTSimple,
    XTPoly,
    XTArrow,
    XTApp,
    XTExt,
    XPWild,
    XPCons,
    XPLit,
    XPId,
    XPExt,
    XLChar,
    XLString,
    XLInt,
    XLDouble,
    XLExt,
    XLam,
    XApp,
    XBinary,
    XVar,
    XLit,
    XAssign,
    XIf,
    XMatch,
    XAnn,
    XBlock,
    XExt,
    XTcSum,
    XTcRecord,
    XTcSyn,
    XTcExt,
    XProg,
    XLet,
    XExternal,
    XType,
    getId
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Pretty.Tree (Node (..), SimpleTree (..))


data NoExt = NoExt

data Name x = Name (XName x) Text

data Binder x
  = Typed (XBTyped x) (Pattern x) (Typer x)
  | Raw (XBRaw x) (Pattern x)

data Typer x
  = TSimple (XTSimple x) (Name x)
  | TPoly (XTPoly x) (Name x)
  | TArrow (XTArrow x) (Typer x) (Typer x)
  | TApp (XTApp x) (Typer x) (Typer x)
  | TForall (XTForall x) (Name x) (Typer x)
  | TExt !(XTExt x)

data Pattern x
  = PWild (XPWild x)
  | PCons (XPCons x) (Name x) [Pattern x]
  | PId (XPId x) (Name x)
  | PLit (XPLit x) (Literal x)
  | PExt !(XPExt x)

data Literal x
  = LChar (XLChar x) Char
  | LString (XLString x) Text
  | LInt (XLInt x) Integer
  | LDouble (XLDouble x) Double
  | LExt !(XLExt x)

data Assign x = Assign
  { assignPos :: XAssign x,
    assignName :: Binder x,
    assignVal :: Expr x
  }

data Sttms x
  = End (Expr x)
  | SAssign (Assign x) (Sttms x)
  | SExpr (Expr x) (Sttms x)

data Expr x
  = Lam (XLam x) (Binder x) (Expr x)
  | App (XApp x) (Expr x) (Expr x)
  | Var (XVar x) (Name x)
  | Lit (XLit x) (Literal x)
  | Block (XBlock x) (Sttms x)
  | If (XIf x) (Expr x) (Expr x) (Expr x)
  | Match (XMatch x) (Expr x) [(Pattern x, Expr x)]
  | Binary (XBinary x) (Name x) (Expr x) (Expr x)
  | Ann (XAnn x) (Expr x) (Typer x)
  | Ext !(XExt x)

data TypeCons x
  = TcSum (XTcSum x) [(Name x, [Typer x])]
  | TcRecord (XTcRecord x) [(Name x, Typer x)]
  | TcSyn (XTcSyn x) (Typer x)
  | TcExt !(XTcExt x)

data TypeDecl x = TypeDecl
  { typName :: Name x,
    typArgs :: [Name x],
    typCons :: TypeCons x,
    typExt :: !(XType x)
  }

data LetDecl x = LetDecl
  { letName :: Name x,
    letArgs :: [Binder x],
    letReturn :: Maybe (Typer x),
    letBody :: Expr x,
    letExt :: !(XLet x)
  }

data ExternalDecl x = ExternalDecl
  { extName :: Name x,
    extType :: Typer x,
    extStr :: Text,
    extExt :: !(XExternal x)
  }

data ImportDecl x = ImportDecl
  { impModule :: [Name x],
    impMode :: Either (Name x) [Name x]
  }

data Program x = Program
  { progExternal :: [ExternalDecl x],
    progLet :: [LetDecl x],
    progType :: [TypeDecl x],
    progImport :: [ImportDecl x]
  }

-- Parser Helper

data TLKind x
  = TTypeDecl (TypeDecl x)
  | TLetDecl (LetDecl x)
  | TExternalDecl (ExternalDecl x)

-- Deriving

instance Show NoExt where show _ = ""

-- Type family instances

type family XName x

type family XBTyped x

type family XBRaw x

type family XTForall x

type family XTSimple x

type family XTPoly x

type family XTArrow x

type family XTApp x

type family XTExt x

type family XPWild x

type family XPCons x

type family XPLit x

type family XPId x

type family XPExt x

type family XLChar x

type family XLString x

type family XLInt x

type family XLDouble x

type family XLExt x

type family XLam x

type family XApp x

type family XBinary x

type family XVar x

type family XLit x

type family XAssign x

type family XIf x

type family XMatch x

type family XAnn x

type family XBlock x

type family XExt x

type family XTcSum x

type family XTcRecord x

type family XTcSyn x

type family XTcExt x

type family XProg x

type family XLet x

type family XExternal x

type family XType x

-- Pretty printing

getId :: Name e -> Text
getId (Name _ k) = k

stmtToList :: Sttms x -> [Node]
stmtToList (End expr) = [toTree expr]
stmtToList (SExpr assign sttms) = toTree assign : stmtToList sttms
stmtToList (SAssign expr sttms) = toTree expr : stmtToList sttms

instance SimpleTree (Assign x) where
  toTree (Assign _ name val) = Node "Assign" [toTree name, toTree val]

instance SimpleTree (Name x) where
  toTree (Name _ n) = Node ("Name: " ++ Text.unpack n) []

instance SimpleTree (Binder x) where
  toTree (Typed _ pat ty) = Node "Typed" [toTree pat, toTree ty]
  toTree (Raw _ pat) = Node "Raw" [toTree pat]

instance SimpleTree (Typer x) where
  toTree (TSimple _ name) = Node "TSimple" [toTree name]
  toTree (TPoly _ name) = Node "TPoly" [toTree name]
  toTree (TArrow _ a b) = Node "TArrow" [toTree a, toTree b]
  toTree (TApp _ ty ty') = Node "TApp" [toTree ty, toTree ty']
  toTree (TForall _ b ty) = Node "TForall" [toTree b, toTree ty]
  toTree (TExt _) = Node "TExt" []

instance SimpleTree (Pattern x) where
  toTree (PWild _) = Node "PWild" []
  toTree (PCons _ a pat) = Node "PCons" [toTree a, toTree pat]
  toTree (PId _ name) = Node "PId" [toTree name]
  toTree (PLit _ lit) = Node "PLit" [toTree lit]
  toTree (PExt _) = Node "PExt" []

instance SimpleTree (Literal x) where
  toTree (LChar _ _) = Node "LChar" []
  toTree (LString _ s) = Node ("LString: " ++ show s) []
  toTree (LInt _ _) = Node "LInt" []
  toTree (LDouble _ d) = Node ("LDouble: " ++ show d) []
  toTree (LExt _) = Node "LExt" []

instance SimpleTree (Expr x) where
  toTree (Lam _ binder expr) = Node "Lam" [toTree binder, toTree expr]
  toTree (App _ e e2) = Node "App" [toTree e, toTree e2]
  toTree (Var _ name) = Node "Var" [toTree name]
  toTree (Lit _ lit) = Node "Lit" [toTree lit]
  toTree (If _ a b c) = Node "If" [toTree a, toTree b, toTree c]
  toTree (Block _ sttms) = Node "block" $ stmtToList sttms
  toTree (Match _ match expr) = Node "Expr" [toTree match, toTree expr]
  toTree (Binary _ name e e2) = Node "Binary" [toTree name, toTree e, toTree e2]
  toTree (Ann _ a b) = Node "Abb" [toTree a, toTree b]
  toTree (Ext _) = Node "Ext" []

instance SimpleTree (TypeCons x) where
  toTree (TcSum _ fields) = Node "TcSum" (map toTree fields)
  toTree (TcRecord _ fields) = Node "TcReco" (map toTree fields)
  toTree (TcSyn _ ty) = Node "TcSyn" [toTree ty]
  toTree (TcExt _) = Node "TcExt" []

instance SimpleTree (TypeDecl x) where
  toTree (TypeDecl name args cons _) =
    Node "TypeDecl" [toTree name, toTree args, toTree cons]

instance SimpleTree (LetDecl x) where
  toTree (LetDecl name args ret body _) =
    Node "LetDecl" [toTree name, toTree args, toTree ret, toTree body]

instance SimpleTree (ExternalDecl x) where
  toTree (ExternalDecl name ty str _) =
    Node "ExternalDecl" [toTree name, toTree ty, toTree str]

instance SimpleTree (ImportDecl x) where
  toTree (ImportDecl _ mode) =
    Node "ImportDecl" [Node "Module" [], toTree mode]

instance SimpleTree (Program x) where
  toTree (Program ex le ty imp) =
    Node "Program" [toTree ex, toTree le, toTree ty, toTree imp]
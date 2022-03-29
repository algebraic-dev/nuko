{-| Base module for every Syntax tree in "Trees that grow" format that 
    will be used in the project. Probably this project will have only 
    a few types of abstract trees

    - Normal: Only positions are stored inside the tree
    - Typed: Positions and types together
    - Lowered: No types or patterns are included here, it's an untyped ast
    - Optimized: No lambdas or things like that, it will have a few structures
-}
module Syntax.Expr where

import Data.Text (Text)
import Pretty.Tree ( Node(..), SimpleTree(..) )
import qualified Data.Text as Text

data NoExt = NoExt

data Name ζ = Name (XName ζ) Text

data Binder ζ
    = Typed (XBTyped ζ) (Pattern ζ) (Typer ζ)
    | Raw (XBRaw ζ) (Pattern ζ)

data Typer ζ
    = TSimple (XTSimple ζ) (Name ζ)
    | TPoly (XTPoly ζ) (Name ζ)
    | TArrow (XTArrow ζ) (Typer ζ) (Typer ζ)
    | TApp (XTApp ζ) (Typer ζ) (Typer ζ)
    | TForall (XTForall ζ) (Name ζ) (Typer ζ)
    | TExt !(XTExt ζ)

data Pattern ζ
    = PWild (XPWild ζ)
    | PCons (XPCons ζ) (Name ζ) [Pattern ζ]
    | PId  (XPId ζ) (Name ζ)
    | PLit (XPLit ζ) (Literal ζ)
    | PExt !(XPExt ζ)

data Literal ζ
    = LChar (XLChar ζ) Char
    | LString (XLString ζ) Text
    | LInt (XLInt ζ) Integer
    | LDouble (XLDouble ζ) Double
    | LExt !(XLExt ζ)

data Assign ζ = Assign { assignPos :: XAssign ζ
                       , assignName :: Binder ζ
                       , assignVal :: Expr ζ }

data Sttms ζ
    = End (Expr ζ)
    | SAssign (Assign ζ) (Sttms ζ)
    | SExpr (Expr ζ) (Sttms ζ)

data Expr ζ
    = Lam (XLam ζ) (Binder ζ) (Expr ζ)
    | App (XApp ζ) (Expr ζ) (Expr ζ)
    | Var (XVar ζ) (Name ζ)
    | Lit (XLit ζ) (Literal ζ)
    | Block (XBlock ζ) (Sttms ζ)
    | If (XIf ζ) (Expr ζ) (Expr ζ) (Expr ζ)
    | Match (XMatch ζ) (Expr ζ) [(Pattern ζ, Expr ζ)]
    | Binary (XBinary ζ) (Name ζ) (Expr ζ) (Expr ζ)
    | Ann (XAnn ζ) (Expr ζ) (Typer ζ)
    | Ext !(XExt ζ)

data TypeCons ζ
    = TcSum (XTcSum ζ) [(Name ζ, [Typer ζ])]
    | TcRecord (XTcRecord ζ) [(Name ζ, Typer ζ)]
    | TcSyn (XTcSyn ζ) (Typer ζ)
    | TcExt !(XTcExt ζ)

data TypeDecl ζ
    = TypeDecl { typName   :: Name ζ
               , typArgs   :: [Name ζ]
               , typCons   :: TypeCons ζ
               , typExt    :: !(XType ζ) }

data LetDecl ζ
    = LetDecl { letName   :: Name ζ
              , letArgs   :: [Binder ζ]
              , letReturn :: Maybe (Typer ζ)
              , letBody   :: Expr ζ
              , letExt        :: !(XLet ζ) }

data ExternalDecl ζ
    = ExternalDecl { extName   :: Name ζ
                   , extType   :: Typer ζ
                   , extStr    :: Text
                   , extExt    :: !(XExternal ζ) }

data ImportDecl ζ
    = ImportDecl { impModule :: [Name ζ]
                 , impMode   :: Either (Name ζ) [Name ζ]}

data Program ζ
    = Program { progExternal :: [ExternalDecl ζ]
              , progLet    :: [LetDecl ζ]
              , progType   :: [TypeDecl ζ]
              , progImport   :: [ImportDecl ζ] }

-- Parser Helper

data TLKind ζ = TTypeDecl (TypeDecl ζ)
              | TLetDecl (LetDecl ζ)
              | TExternalDecl (ExternalDecl ζ)

-- Deriving 

instance Show NoExt where  show _ = ""

-- Type family instances

type family XName ζ

type family XBTyped ζ
type family XBRaw ζ

type family XTForall ζ
type family XTSimple ζ
type family XTPoly ζ
type family XTArrow ζ
type family XTApp ζ
type family XTExt ζ

type family XPWild ζ
type family XPCons ζ
type family XPLit ζ
type family XPId ζ
type family XPExt ζ

type family XLChar ζ
type family XLString ζ
type family XLInt ζ
type family XLDouble ζ
type family XLExt ζ

type family XLam ζ
type family XApp ζ
type family XBinary ζ
type family XVar ζ
type family XLit ζ
type family XAssign ζ
type family XIf ζ
type family XMatch ζ
type family XAnn ζ
type family XBlock ζ
type family XExt ζ

type family XTcSum ζ
type family XTcRecord ζ
type family XTcSyn ζ
type family XTcExt ζ

type family XProg ζ
type family XLet ζ
type family XExternal ζ
type family XType ζ

-- Pretty printing

getId :: Name e -> Text 
getId (Name _ k) = k

stmtToList :: Sttms ζ -> [Node]
stmtToList (End expr) = [toTree expr]
stmtToList (SExpr assign sttms) = toTree assign : stmtToList sttms
stmtToList (SAssign expr sttms)   = toTree expr : stmtToList sttms

instance SimpleTree (Assign ζ) where
    toTree (Assign _ name val) = Node "Assign" [toTree name, toTree val]

instance SimpleTree (Name ζ) where
    toTree (Name _ n) = Node ("Name: " ++ Text.unpack n) []

instance SimpleTree (Binder ζ) where
    toTree (Typed _ pat ty) = Node "Typed" [toTree pat, toTree ty]
    toTree (Raw _ pat) = Node "Raw" [toTree pat]

instance SimpleTree (Typer ζ) where
    toTree (TSimple _ name) = Node "TSimple" [toTree name]
    toTree (TPoly _ name) = Node "TPoly" [toTree name]
    toTree (TArrow _ a b) = Node "TArrow" [toTree a, toTree b]
    toTree (TApp _ ty ty') = Node "TApp" [toTree ty, toTree ty']
    toTree (TForall _ b ty) = Node "TForall" [toTree b, toTree ty]
    toTree (TExt _) = Node "TExt" []

instance SimpleTree (Pattern ζ) where
    toTree (PWild _) = Node "PWild" []
    toTree (PCons _ a pat) = Node "PCons" [toTree a, toTree pat]
    toTree (PId _ name) = Node "PId" [toTree name]
    toTree (PLit _ lit) = Node "PLit" [toTree lit]
    toTree (PExt _) = Node "PExt" []

instance SimpleTree (Literal ζ) where
    toTree (LChar _ _) = Node "LChar" []
    toTree (LString _ s) = Node ("LString: " ++ show s) []
    toTree (LInt _ _) = Node "LInt" []
    toTree (LDouble _ d) = Node ("LDouble: " ++ show d) []
    toTree (LExt _) = Node "LExt" []

instance SimpleTree (Expr ζ) where
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

instance SimpleTree (TypeCons ζ) where
    toTree (TcSum _ fields) = Node "TcSum" (map toTree fields)
    toTree (TcRecord _ fields) = Node "TcReco" (map toTree fields)
    toTree (TcSyn _ ty) = Node "TcSyn" [toTree ty]
    toTree (TcExt _) = Node "TcExt" []

instance SimpleTree (TypeDecl ζ) where
    toTree (TypeDecl name args cons _) =
        Node "TypeDecl" [toTree name, toTree args, toTree cons]

instance SimpleTree (LetDecl ζ) where
    toTree (LetDecl name args ret body _) =
        Node "LetDecl" [toTree name, toTree args, toTree ret, toTree body]

instance SimpleTree (ExternalDecl ζ) where
    toTree (ExternalDecl name ty str _) =
        Node "ExternalDecl" [toTree name, toTree ty, toTree str]

instance SimpleTree (ImportDecl ζ) where
    toTree (ImportDecl _ mode) =
        Node "ImportDecl" [Node "Module" [], toTree mode]

instance SimpleTree (Program ζ) where
    toTree (Program ex le ty imp) =
        Node "Program" [toTree ex, toTree le, toTree ty, toTree imp]
module Syntax.Expr where 

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)

data NoExt = NoExt

data Name ζ = Name (XName ζ) Text

data Visibility = Public | Private

data Binder ζ 
    = Typed (XBTyped ζ) (Pattern ζ) (Type ζ) 
    | Raw (XBRaw ζ) (Pattern ζ) 

data Type ζ
    = TSimple (XTSimple ζ) (Name ζ)
    | TPoly (XTPoly ζ) (Name ζ) 
    | TArrow (XTArrow ζ) (Type ζ) (Type ζ)
    | TCons (XTCons ζ) (Name ζ) [Type ζ]
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

data Expr ζ
    = Lam (XLam ζ) (Binder ζ) (Expr ζ)
    | App (XApp ζ) (Expr ζ) (Expr ζ)
    | Var (XVar ζ) (Name ζ) 
    | Lit (XLit ζ) (Literal ζ)
    | Assign (XAssign ζ) (Binder ζ) [Expr ζ] 
    | Match (XMatch ζ) (Expr ζ) [(Pattern ζ, Expr ζ)]
    | Ext !(XExt ζ)

data TypeCons ζ
    = TcSum (XTcSum ζ) [(Name ζ, [Type ζ])]
    | TcRecord (XTcRecord ζ) [(Name ζ, Type ζ)]
    | TcSyn (XTcSyn ζ) (Type ζ)
    | TcExt !(XTcExt ζ)

data TypeDecl ζ
    = TypeDecl { typName   :: Name ζ
               , typArgs   :: [Name ζ]
               , typCons   :: TypeCons ζ
               , typExt    :: !(XType ζ) }

data LetDecl ζ
    = LetDecl { letName   :: Name ζ
              , letArgs   :: [Binder ζ]
              , letReturn :: Maybe (Type ζ)
              , letBody   :: [Expr ζ]
              , letExt        :: !(XLet ζ) }

data ExportDecl ζ
    = ExportDecl { expName   :: Name ζ
                 , expArgs   :: [Binder ζ]
                 , expStr    :: Text
                 , extExt        :: !(XExt ζ) }

data Program ζ
    = Program { progExport :: [ExportDecl ζ]
              , progLet    :: [LetDecl ζ]
              , progType   :: [TypeDecl ζ] }

-- Parser Helper

data TLKind ζ = TTypeDecl (TypeDecl ζ)
              | TLetDecl (LetDecl ζ)
              | TExportDecl (ExportDecl ζ)

-- Deriving 

instance Show (NoExt) where 
    show _ = ""

-- Type family instances

type family XName ζ

type family XBTyped ζ
type family XBRaw ζ

type family XTSimple ζ
type family XTPoly ζ
type family XTArrow ζ
type family XTCons ζ
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
type family XVar ζ
type family XLit ζ
type family XAssign ζ
type family XMatch ζ
type family XBlock ζ
type family XExt ζ

type family XTcSum ζ
type family XTcRecord ζ
type family XTcSyn ζ
type family XTcExt ζ

type family XProg ζ
type family XLet ζ
type family XExport ζ
type family XType ζ

-- Pretty printing


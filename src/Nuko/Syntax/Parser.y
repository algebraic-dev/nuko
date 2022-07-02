{

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module Nuko.Syntax.Parser where

import Relude

import Nuko.Syntax.Lexer.Support
import Nuko.Syntax.Lexer.Tokens
import Nuko.Syntax.Lexer
import Nuko.Syntax.Range
import Nuko.Syntax.Range
import Nuko.Syntax.Tree
import Nuko.Syntax.Error
import Nuko.Utils
import Nuko.Tree

import Data.Text (Text)
import Data.Bifunctor

import qualified Prelude                 as Prelude
import qualified Control.Monad.Chronicle as Chronicle
import qualified Data.List.NonEmpty      as NE

}

%name parseProgram Program

%tokentype { Ranged Token }
%monad { Lexer }
%lexer { lexer } { Ranged TcEOF _ }

%error { parseError }

%token
    type     { Ranged TcType _ }
    match    { Ranged TcMatch _ }
    with     { Ranged TcWith _ }
    let      { Ranged TcLet _ }
    import   { Ranged TcImport _ }
    as       { Ranged TcAs _ }
    if       { Ranged TcIf _ }
    then     { Ranged TcThen _ }
    else     { Ranged TcElse _ }
    pub      { Ranged TcPub _ }
    forall   { Ranged TcForall _ }

    int      { Ranged (TcInt _) _ }
    str      { Ranged (TcStr _) _ }

    lower    { Ranged (TcLowerId _) _ }
    upper    { Ranged (TcUpperId _) _ }

    '{'      { Ranged TcLBracket _ }
    '}'      { Ranged TcRBracket _ }
    '['      { Ranged TcLBrace _ }
    ']'      { Ranged TcRBrace _ }
    '->'     { Ranged TcArrow _ }
    '=>'     { Ranged TcDoubleArrow _ }
    '('      { Ranged TcLPar _ }
    ')'      { Ranged TcRPar _ }
    ':'      { Ranged TcColon _ }
    '='      { Ranged TcEqual _ }
    '|'      { Ranged TcPipe _ }
    ','      { Ranged TcComma _ }
    '\\'     { Ranged TcSlash _ }
    '.'      { Ranged TcDot _ }
    '_'      { Ranged TcWild _ }

    begin    { Ranged TcBegin _ }
    sep      { Ranged TcSep _ }
    end      { Ranged TcEnd _ }

%right '->'

%%

-- Helpful predicates

SepList(Sep, Pred)
    : SepListHelper(Sep, Pred)     { $1 }
    | {- UwU Empty -}              { [] }

SepListHelper(Sep, Pred)
    : Pred  Sep SepListHelper(Sep, Pred) { $1 : $3 }
    | Pred                               { $1 : [] }

SepList1(Sep, Pred)
    : Pred  Sep SepListHelper(Sep, Pred) { $1 :| $3 }
    | Pred                               { $1 :| [] }

List1(Pred)
    : Pred List1(Pred)  { $1 NE.<| $2 }
    | Pred              { NE.singleton $1 }

List(Pred)
    : Pred List(Pred) { $1 : $2 }
    | {- UwU Empty -} { [] }

Optional(Pred)
    : Pred            { Just $1 }
    | {- Empty UwU -} { Nothing }

-- Parsing of identifiers and paths

Lower :: { Name } : lower { Name (getData $1) $1.position }
Upper :: { Name } : upper { Name (getData $1) $1.position }

PathHelper(Pred)
    : Upper '.' PathHelper(Pred) { let (p, f) = $3 in ($1 : p, f) }
    | Pred { ([], $1) }

PathEnd
    : Upper           { \p -> withPosListR p $1 (Upper $ withPosList $1 p (Path p $1)) }
    | Lower           { \p -> withPosListR p $1 (Lower $ withPosList $1 p (Path p $1)) }
    | Lower '.' Lower { \p -> withPosListR p $3 (Field (withPosListR p $1 $ Lower (withPosList $1 p $ Path p $1)) $3) }

PathExpr : PathHelper(PathEnd) { let (p , f) = $1 in f p }

Path(Pred) : PathHelper(Pred) { let (p , f) = $1 in withPosListR p f $ Path p f }

-- Types

TypeAtom :: { Ty Nm }
    : Lower        { TPoly $1 NoExt }
    | Path(Upper)  { TId $1 NoExt }
    | '(' Type ')' { $2 }

TypeCon :: { Ty Nm }
    : Path(Upper) List1(TypeAtom) { withPos $1 $2 $ TCons $1 $2 }
    | TypeCon '->' TypeCon        { withPos $1 $3 $ TArrow $1 $3 }
    | TypeAtom                    { $1 }

Type
    : TypeCon                    { $1 }
    | forall Lower '.' Type      { withPos $1 $4 $ TForall $2 $4 }

-- Patterns

AtomPat :: { Pat Nm }
    : '_'                         { PWild $1.position }
    | Lower                       { PId $1 NoExt }
    | Literal                     { PLit $1 NoExt }
    | '(' Pat ')'                 { $2 }

Pat :: { Pat Nm }
    : Path(Upper) List(AtomPat) { withPosList $1 $2 $ PCons $1 $2 }
    | Pat ':' Type              { withPos $1 $3     $ PAnn $1 $3 }
    | AtomPat                   { $1 }

-- Expressions

OptSep : Optional(sep) { () }

Literal :: { Literal Nm }
    : int  { LInt (getInt $1) $1.position }
    | str  { LStr (getData $1) $1.position }

Atom :: { Expr Nm }
    : PathExpr     { $1 }
    | Literal      { Lit $1 NoExt }
    | '(' Expr ')' { $2 }

App :: { NE.NonEmpty (Expr Nm) }
    : Atom App { $1 NE.<| $2 }
    | Atom      { $1 NE.:| [] }

VarExpr :: { Var Nm }
    : let Pat '=' Expr  { withPos $1 $4 $ Var $2 $4 }

BlockExpr :: { Block Nm }
    : Expr       List1(sep) BlockExpr { BlBind $1 $3 }
    | VarExpr    List1(sep) BlockExpr { BlVar $1 $3 }
    | Expr                            { BlEnd $1 }
    | VarExpr                         {% flag (AssignInEndOfBlock $1.ext)
                                      >> pure (BlEnd $1.val) }

End : end   { ()         }
    | error {% popLayout }

-- Match can be sucessed of a End so.. sometimes it will give an layout error
ClosedExpr :: { Expr Nm }
    : if ClosedExpr OptSep then ClosedExpr OptSep else ClosedExpr { withPos $1 $8 $ If $2 $5 (Just $8) }
    | Atom App                                                    { withPos $1 $2 $ App $1 $2 }
    | match ClosedExpr with begin SepList1(sep, CaseClause) End   { withPos $1 $5 $ Match $2 $5 }
    | '\\' Pat '=>' ClosedExpr                                    { withPos $1 $4 $ Lam $2 $4 }
    | begin BlockExpr End                                         { case $2 of { BlEnd x -> x; _ -> withPos $1 $2 $ Block $2} }
    | Atom                                                        { $1 }

Expr :: { Expr Nm }
    : ClosedExpr                                         { $1 }
    | ClosedExpr ':' Type                                { withPos $1 $3 $ Ann $1 $3 }

CaseClause :: { ((Pat Nm, Expr Nm)) }
    : Pat '=>' Expr { ($1, $3) }

-- Top level

ProdClause :: { (Name, Ty Nm) }
    : Lower ':' Type { ($1, $3) }

SumClause :: { (Name, [Ty Nm]) }
    : '|' Upper List(TypeAtom) { ($2, $3) }

TypeTy :: { TypeDeclArg Nm }
    : '{' SepList(',', ProdClause) '}'  { TypeProd $2 }
    | List1(SumClause)                  { TypeSum $1 }
    | Type                              { TypeSym $1 }

TypeDecl : type Upper List(Lower) '=' TypeTy { TypeDecl $2 $3 $5 }

Ret : ':' Type { $2 }

Binder : '(' Lower ':' Type ')' { ($2, $4) }

LetDecl : let Lower List(Binder) Optional(Ret) '=' Expr { LetDecl $2 $3 $6 $4 NoExt }

-- Import declaration

ImpPath :: { NonEmpty Name }
    : SepList1('.', Upper) { $1 }

ImpDeps :: { ImportDeps Nm }
    : Upper as Lower {% terminate (WrongUsageOfCase LowerCase $3.range) }
    | Lower as Upper {% terminate (WrongUsageOfCase UpperCase $3.range) }
    | Upper as Upper { ImportDeps (ImpDepUpper $1) (Just $3) }
    | Lower as Lower { ImportDeps (ImpDepLower $1) (Just $3) }
    | Upper          { ImportDeps (ImpDepUpper $1) Nothing }
    | Lower          { ImportDeps (ImpDepLower $1) Nothing }

Imp :: { ImportModifier Nm }
    : '(' SepList1(',', ImpDeps) ')' { ImpList $2 }
    | as Upper                       { ImpAs $2 }

ImportDecl : import ImpPath Optional(Imp) { Import $2 $3 NoExt }

Program :: { Program Nm }
    : LetDecl Program    { $2 { letDecls  = $1 : $2.letDecls }  }
    | TypeDecl Program   { $2 { typeDecls = $1 : $2.typeDecls } }
    | ImportDecl Program { $2 { impDecls  = $1 : $2.impDecls }  }
    | {- Empty UwU -}    { Program [] [] [] NoExt }
{

withPosList :: (HasPosition a, HasPosition b) => a -> [b] -> (Range -> c) -> c
withPosList p []       fn = fn (getPos p)
withPosList p (x : xs) fn = fn (getPos p <> getPos (Relude.last (x :| xs)))

withPosListR :: (HasPosition a, HasPosition b) => [b] -> a -> (Range -> c) -> c
withPosListR []     p fn = fn (getPos p)
withPosListR (x: _) p fn = fn (getPos x <> getPos p)

withPos :: (HasPosition a, HasPosition b) => a -> b -> (Range -> c) -> c
withPos p p1 fn = fn (getPos p <> getPos p1)

-- These two fucntions are useful because I want to get the token with the range
-- So I just cannot get the data inside using the "$$" marker inside the %token definition
getData :: Ranged Token -> Text
getData = \case
    (Ranged (TcStr s) _)     -> s
    (Ranged (TcLowerId s) _) -> s
    (Ranged (TcUpperId s) _) -> s
    _ -> error "Chiyoku.. you have to be more careful when you try to use this function!"

getInt :: Ranged Token -> Int
getInt = \case
    (Ranged (TcInt s) _) -> s
    _ -> error "Chiyoku.. you have to be more careful when you try to use this function!"

lexer      = (scan >>=)
parseError = terminate . UnexpectedToken

}
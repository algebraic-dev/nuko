{
module Nuko.Syntax.Parser where

import Nuko.Syntax.Lexer.Support
import Nuko.Syntax.Lexer.Tokens
import Nuko.Syntax.Lexer
import Nuko.Syntax.Range
import Nuko.Syntax.Error
import Nuko.Syntax.Range
import Nuko.Syntax.Ast
import Nuko.Tree.TopLevel
import Nuko.Tree.Expr
import Data.Text            (Text)
import Control.Monad.Except (throwError)

import qualified Data.List.NonEmpty as NE

}

%name parseExpr Expr
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
    : SepList1(Sep, Pred)          { $1 }
    | {- UwU Empty -}             { [] }

SepList1(Sep, Pred)
    : Pred Sep SepList1(Sep, Pred) { $1 : $3 }
    | Pred                        { [$1] }

List1(Pred)
    : Pred List1(Pred) { $1 NE.<| $2 }
    | Pred             { NE.singleton $1 }

List(Pred)
    : Pred List(Pred) { $1 : $2 }
    | {- UwU Empty -} { [] }

Optional(Pred)
    : Pred            { Just $1 }
    | {- Empty UwU -} { Nothing }

-- Help

Lower :: { Name Normal } : lower { Name (getData $1) $1.position }
Upper :: { Name Normal } : upper { Name (getData $1) $1.position }

-- Paths

PathHelper(Pred)
    : Upper '.' PathHelper(Pred) { let (p, f) = $3 in ($1 : p, f) }
    | Pred { ([], $1) }

PathEnd
    : Upper           { \p -> withPosListR p $1 (Lower (Path p $1)) }
    | Lower           { \p -> withPosListR p $1 (Upper (Path p $1)) }
    | Lower '.' Lower { \p -> withPosListR p $3 (Accessor (withPosListR p $1 $ Lower (Path p $1)) $3) }

PathExpr : PathHelper(PathEnd) { let (p , f) = $1 in f p }

Path(Pred) : PathHelper(Pred) { let (p , f) = $1 in Path p f }

-- Types

TypeAtom :: { Type Normal }
    : Lower        { TPoly $1 NoExt }
    | Path(Upper)  { TId $1 NoExt }
    | '(' Type ')' { $2 }

TypeCon :: { Type Normal }
    : Path(Upper) List1(TypeAtom) { withPos $1 $2 $ TCons $1 $2 }
    | TypeCon '->' TypeCon        { withPos $1 $3 $ TArrow $1 $3 }
    | TypeAtom                    { $1 }

Type
    : TypeCon                    { $1 }
    | forall Lower '.' Type      { withPos $1 $4 $ TForall $2 $4 }
-- Patterns

AtomPat :: { Pat Normal }
    : '_'                         { PWild $1.position }
    | Lower                       { PId $1 NoExt }
    | Literal                     { PLit $1 NoExt }
    | '(' Pat ')'                 { $2 }

Pat :: { Pat Normal }
    : Path(Upper) List(AtomPat) { withPosList $1 $2 $ PCons $1 $2 }
    | Pat ':' Type              { withPos $1 $3     $ PAnn $1 $3 }
    | AtomPat                   { $1 }

-- Exprs

OptSep : Optional(sep) { () }

Literal :: { Literal Normal }
    : int  { LInt (getInt $1) $1.position }
    | str  { LStr (getData $1) $1.position }

Atom :: { Expr Normal }
    : PathExpr     { $1 }
    | Literal      { Lit $1 NoExt }
    | '(' Expr ')' { $2 }

App :: { NE.NonEmpty (Expr Normal) }
    : Atom App { $1 NE.<| $2 }
    | Atom      { $1 NE.:| [] }

VarExpr :: { Var Normal }
    : let Pat '=' Expr               { withPos $1 $4 $ Var $2 $4 }

BlockExpr :: { Block Normal }
    : Expr       sep BlockExpr { BlBind $1 $3 }
    | VarExpr    sep BlockExpr { BlVar $1 $3 }
    | VarExpr                  {% throwError (CannotAssign $ getPos $1) }
    | Expr                     { BlEnd $1 }

End : end   { ()         }
    | error {% popLayout }

CaseClause :: { ((Pat Normal, Expr Normal)) }
    : Pat '=>' Expr { ($1, $3) }

-- Match can be sucessed of a End so.. sometimes it will give an layout error
ClosedExpr :: { Expr Normal }
    : if ClosedExpr OptSep then ClosedExpr OptSep else ClosedExpr { withPos $1 $8 $ If $2 $5 (Just $8) }
    | match ClosedExpr with begin SepList(sep, CaseClause) End    { withPos $1 $5 $ Case $2 $5 }
    | Atom App                                          { withPos $1 $2 $ App $1 $2 }
    | '\\' Pat '=>' ClosedExpr                           { withPos $1 $4 $ Lam $2 $4 }
    | begin BlockExpr End                                { case $2 of { BlEnd x -> x; _ -> withPos $1 $2 $ Block $2} }
    | Atom                                               { $1 }

Expr :: { Expr Normal }
    : ClosedExpr                                         { $1 }
    | ClosedExpr ':' Type                                { withPos $1 $3 $ Ann $1 $3 }

-- Top Level

ProdClause :: { (Name Normal, Type Normal) }
    : Lower ':' Type { ($1, $3) }

SumClause :: { (Name Normal, [Type Normal]) }
    : '|' Upper List(TypeAtom) { ($2, $3) }

TypeTy :: { TypeDeclArg Normal }
    : '{' SepList(',', ProdClause) '}' { TypeProd $2 }
    | List1(SumClause)                 { TypeSum $1 }
    | Type                             { TypeSym $1 }

TypeDecl : type Upper List(Lower) '=' TypeTy { TypeDecl $2 $3 $5 }

Ret : ':' Type { $2 }

LetDecl : let Lower List(AtomPat) Optional(Ret) '=' Expr { LetDecl $2 $3 $6 $4 NoExt }

Program :: { Program Normal }
    : LetDecl Program  { $2 { letDecls = $1 : $2.letDecls } }
    | TypeDecl Program { $2 { tyDecls = $1  : $2.tyDecls } }
    | {- Empty UwU -}  { Program [] [] NoExt }

{

withPosList :: (HasPosition a, HasPosition b) => a -> [b] -> (Range -> c) -> c
withPosList p [] fn = fn (getPos p)
withPosList p xs fn = fn (getPos p <> getPos xs)

withPosListR :: (HasPosition a, HasPosition b) => [b] -> a -> (Range -> c) -> c
withPosListR []     p fn = fn (getPos p)
withPosListR (x: _) p fn = fn (getPos x <> getPos p)

withPos :: (HasPosition a, HasPosition b) => a -> b -> (Range -> c) -> c
withPos p p1 fn = fn (getPos p <> getPos p1)

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
parseError = throwError . UnexpectedToken

}
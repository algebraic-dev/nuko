{
module Nuko.Syntax.Parser where

import Nuko.Syntax.Lexer.Support
import Nuko.Syntax.Lexer.Tokens
import Nuko.Syntax.Lexer
import Nuko.Syntax.Range
import Nuko.Syntax.Error
import Nuko.Syntax.Range
import Nuko.Syntax.Ast
import Nuko.Tree.Expr
import Data.Text            (Text)
import Control.Monad.Except (throwError)

import qualified Data.List.NonEmpty as NE

}

%name parseExpr Expr

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

%%

-- Helpful predicates

SepList(Sep, Expr)
    : Expr Sep SepList(Sep, Expr) { $1 : $3 }
    | Expr                        { [$1] }

List(Expr)
    : Expr List(Expr) { $1 : $2 }
    | Expr { [$1] }

Optional(Expr)
    : Expr            { Just $1 }
    | {- Empty UwU -} { Nothing }

-- Help

Lower :: { Name Normal } : lower { Name (getData $1) $1.position }
Upper :: { Name Normal } : upper { Name (getData $1) $1.position }

-- Paths

PathHelper(Pred)
    : Upper '.' PathHelper(Pred) { let (p, f) = $3 in ($1 : p, f) }
    | Pred { ([], $1) }

PathEnd
    : Upper           { \p -> withPos p $1 (Lower (Path p $1)) }
    | Lower           { \p -> withPos p $1 (Upper (Path p $1)) }
    | Lower '.' Lower { \p -> withPos p $3 (Accessor (withPos p $1 $ Lower (Path p $1)) $3) }

PathExpr : PathHelper(PathEnd) { let (p , f) = $1 in f p }

Path(Pred) : PathHelper(Pred) { let (p , f) = $1 in Path p f }

-- Patterns

AtomPat :: { Pat Normal }
    : '_'                         { PWild $1.position }
    | Lower                       { PId $1 NoExt }
    | Literal                     { PLit $1 NoExt }
    | '(' Pat ')'                 { $2 }

Pat :: { Pat Normal }
    : Path(Upper) List(AtomPat) { undefined }

-- Exprs

Literal :: { Literal Normal }
    : int  { LInt (getInt $1) $1.position }
    | str  { LStr (getData $1) $1.position }

Atom :: { Expr Normal }
    : PathExpr     { $1 }
    | Literal      { Lit $1 NoExt }
    | '(' Expr ')' { $2 }

Call :: { NE.NonEmpty (Expr Normal) }
    : Atom Call { $1 NE.<| $2 }
    | Atom      { $1 NE.:| [] }

BlockExpr :: { Block Normal }
    : ClosedExpr       sep BlockExpr { BlBind $1 $3 }
    | let Pat '=' Expr sep BlockExpr { BlVar (withPos $1 $4 $ Var $2 $4) $6 }
    | ClosedExpr                     { BlEnd $1 }

End : end   { ()         }
    | error {% popLayout }

ClosedExpr :: { Expr Normal }
    : if ClosedExpr then ClosedExpr else ClosedExpr { withPos $1 $6 $ If $2 $4 (Just $6) }
    | Atom Call                                     { withPos $1 $2 $ Call $1 $2 }
    | Atom                                          { $1 }
    | begin BlockExpr End                           { withPos $1 $2 $ Block $2 }

Expr :: { Expr Normal }
    : ClosedExpr                    { $1 }
    | if ClosedExpr then ClosedExpr { withPos $1 $4 $ If $2 $4 Nothing }

{

withPos :: (HasPosition a, HasPosition b) => a -> b -> (Range -> c) -> c
withPos p p1 fn = fn (mixRange (getPos p) (getPos p1))

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
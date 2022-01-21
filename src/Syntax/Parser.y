{
module Syntax.Parser where 

import Syntax.Lexer.Support (Lexer)
import Syntax.Lexer.Tokens (Token(..))
import Syntax.Lexer (scan)

import Syntax.Expr
import Syntax.Parser.AST

import Syntax.Bounds (WithBounds(WithBounds), position, Bounds)
import Data.Text (Text)

import Data.Function (on)
import Control.Monad.Except (throwError)

}

%name parseProgram Program

%tokentype { WithBounds Token }
%monad { Lexer }
%lexer { lexer } { WithBounds TknEOF _ }

%error { parseError }

%token
    lower  { WithBounds (TknLowerId _) _ }
    upper  { WithBounds (TknUpperId _) _ }
    symbol { WithBounds (TknSymbol _) _ }
    
    number { WithBounds (TknNumber _) _ }
    string { WithBounds (TknLStr _) _ }
    char   { WithBounds (TknLChar _) _ }
    double { WithBounds (TknLDouble _) _ }

    open  { WithBounds TknOpen _ }
    close { WithBounds TknClose _ }
    semi  { WithBounds TknEnd _ }

    '_'   { WithBounds TknWild _ }
    '('   { WithBounds TknLPar _ }
    ')'   { WithBounds TknRPar _ }
    '{'   { WithBounds TknLBrace _ }
    '}'   { WithBounds TknRBrace _ }
    '='   { WithBounds TknEq _ }
    ':'   { WithBounds TknColon _ }
    '|'   { WithBounds TknPipe _ }
    '->'  { WithBounds TknRArrow _ }
    '\\'  { WithBounds TknSlash _ }
    ','   { WithBounds TknComma _ }
    '.'   { WithBounds TknDot _ }

    type   { WithBounds TknKwType _ }
    let    { WithBounds TknKwLet _ }
    do     { WithBounds TknKwDo _ }
    if     { WithBounds TknKwIf _ }
    then   { WithBounds TknKwThen _ }
    else   { WithBounds TknKwElse _ }
    match  { WithBounds TknKwMatch _ }
    with   { WithBounds TknKwWith _ }

%right '->'

%%

Lower : lower { Name (position $1) (getData $1) }
Upper : upper { Name (position $1) (getData $1) }

Lowers : {- empty -} {[]}
       | Lowers Lower { $2 : $1 }
      
ConsPat : Upper Patterns { PCons (getPos $1 <> headOr $2 getPos (getPos $1)) 
                              $1 (reverse $2) }

Patterns :: { [Pattern Normal] }
          : Patterns PatternAtom { $2 : $1 } 
          | {- empty -} { [] }

PatternAtom :: { Pattern Normal }
             : '_'     { PWild (position $1) }
             | Literal { PLit NoExt $1 }
             | Lower   { PId NoExt $1 }
             | '(' RawPat ')' { $2 } 

RawPat :: { Pattern Normal }
        : PatternAtom { $1 }
        | ConsPat { $1 }

Pattern :: { Pattern Normal }
         : PatternAtom { $1 }
         | Upper { PCons (getPos $1) $1 [] }
         | '(' ConsPat ')' { $2 }

TypeAtom :: { Type Normal }
          : Upper { TSimple NoExt $1 }
          | Lower { TPoly NoExt $1 }
          | '(' Type ')' { $2 }

Types :: { [Type Normal] }
       : {- empty -} { [] }
       | Types TypeAtom { $2 : $1 }

Type :: { Type Normal }
      : Type '->' Type { TArrow (getPos $1 <> getPos $3) $1 $3 }
      | TypeAtom { $1 }

Binder :: { Binder Normal }
        : Pattern { Raw NoExt $1 }
        | '(' Pattern ':' Type ')' { Typed ($2 `mix` $4) $2 $4} 

Binders :: { [Binder Normal] }
         : Binders Binder { $2 : $1 } 
         | {- empty -} { [] }

Literal :: { Literal Normal }
         : string { LString (position $1) (getData $1) }
         | number { LInt (position $1) (getDecimal $1) }

Atom :: { Expr Normal }
      : Literal      { Lit NoExt $1 }
      | Lower        { Var NoExt $1 }
      | Upper        { Var NoExt $1 }
      | '(' Expr ')' { $2 }

Call :: { Expr Normal }
      : Call Atom { App ($1 `mix` $2) $1 $2 }
      | Atom { $1 }

Exprs :: { [Expr Normal] }
      : Expr { [$1] } 
      | Exprs semi Expr { $3 : $1 }

MatchClause :: { (Pattern Normal, Expr Normal) }
             : Pattern '->' Expr { ($1, $3) } 

MatchClauses :: { [(Pattern Normal, Expr Normal)] }
              : MatchClause { [$1] }
              | MatchClauses semi MatchClause { $3 : $1 }

Expr :: { Expr Normal }
      : '\\' Binder '->' Expr { Lam ($2 `mix` $4) $2 $4}
      | let Binder '=' open Exprs close { Assign (position $1 <> getPos (head $5)) $2 (reverse $5)}  
      | match Expr with open MatchClauses close { Match (firstAndLast $5) $2 (reverse $5) }
      | Call { $1 }

CoprodClause :: { (Name Normal, [Type Normal])  }
              : '|' Upper Types { ($2, $3) }

CoprodClauses :: { [(Name Normal, [Type Normal])] }
               : CoprodClause { [$1] }
               | CoprodClauses semi CoprodClause { $3 : $1 }

ProdClause :: { (Name Normal, Type Normal) }
            : Lower ':' Type { ($1, $3) }

ProdClauses :: { [(Name Normal, Type Normal)] }
             : ProdClause  { [$1] }
             | ProdClauses semi ProdClause { $3 : $1 }


TypeCons :: { TypeCons Normal }
          : ProdClauses { TcRecord (getPos $ reverse $1) (reverse $1) } 
          | CoprodClauses { TcSum (firstAndLast $1) (reverse $1)}
          | Type { TcSyn NoExt $1 }

TypeDecl : type Upper Lowers '=' open TypeCons close { TypeDecl $2 $3 $6 NoExt }

OptRet : {- empty -} { Nothing }
       | ':' Type { Just $2 } 

LetDecl : let Lower Binders OptRet '=' open Exprs close { LetDecl $2 $3 $4 $7 NoExt }

ProgramDecl : TypeDecl { TTypeDecl $1 }
             | LetDecl  { TLetDecl $1 }

ProgramDecls : ProgramDecls ProgramDecl { $2 : $1 }
             | {- empty -} { [] }

Program : ProgramDecls { foldl filterDecls (Program [] [] []) $1 }

{

filterDecls :: Program x -> TLKind x -> Program x 
filterDecls program (TTypeDecl tyDecl) = program { progType = tyDecl : progType program} 
filterDecls program (TLetDecl tyDecl) = program { progLet = tyDecl : progLet program} 
filterDecls program (TExportDecl tyDecl) = program { progExport = tyDecl : progExport program} 

firstAndLast :: (HasPosition a, HasPosition b) => [(a, b)] -> Bounds
firstAndLast [(a,b)] = a `mix` b
firstAndLast [] = error "zero clauses!"
firstAndLast other =
      case (head other, last other) of 
            ((_, b), (a, _)) -> a `mix` b

headOr :: [a] -> (a -> b) -> b -> b 
headOr [] fn alt       = alt 
headOr (x : xn) fn alt = fn x

mix :: (HasPosition a, HasPosition b) => a -> b -> Bounds
mix a b = getPos (a,b)

getData :: WithBounds Token -> Text 
getData (WithBounds (TknLowerId tx) _) = tx
getData (WithBounds (TknUpperId tx) _) = tx
getData (WithBounds (TknLStr tx) _)    = tx
getData _ = error "error while trying to get data on parser"

getDecimal :: WithBounds Token -> Integer 
getDecimal (WithBounds (TknNumber num) _) = num
getDecimal _ = error "error while trying to get number on parser"

-- Happy primitives

lexer = (scan >>=)
parseError = throwError . show 

}
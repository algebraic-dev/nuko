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

%name parseExpr Lower

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

%%

Lower : lower { Name (position $1) (getData $1) }
Upper : upper { Name (position $1) (getData $1) }

Literal :: { Literal Normal }
         : string { LString (position $1) (getData $1) }
         | number { LInt (position $1) (getDecimal $1) }

Atom :: { Expr Normal }
      : Literal  { Lit NoExt $1 }
      | Lower    { Var NoExt $1 }
      | Upper    { Var NoExt $1 }
      | '(' Expr ')' { $2 }

Call :: { Expr Normal }
      : Call Atom { App ($1 `mix` $2) $1 $2 }
      | Atom { $1 }

Expr :: { Expr Normal }
      : Call { $1 }

{

mix :: HasPosition a => a -> a -> Bounds
mix = (<>) `on` getPos

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
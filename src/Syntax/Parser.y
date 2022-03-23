{
module Syntax.Parser where 

import Syntax.Lexer.Support (Lexer, popLayout)
import Syntax.Lexer.Tokens (Token(..))
import Syntax.Lexer (scan)

import Syntax.Expr
import Syntax.Parser.Ast

import Syntax.Bounds (WithBounds(WithBounds), position, Bounds)
import Data.Text (Text)

import Data.Function (on)
import Control.Monad.Except (throwError)

import qualified Error.Message as ERR
import Debug.Trace 

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

    type     { WithBounds TknKwType _ }
    let      { WithBounds TknKwLet _ }
    import   { WithBounds TknKwImport _ }
    as       { WithBounds TknKwAs _ }
    external { WithBounds TknKwExternal _ }
    if       { WithBounds TknKwIf _ }
    then     { WithBounds TknKwThen _ }
    else     { WithBounds TknKwElse _ }
    match    { WithBounds TknKwMatch _ }
    with     { WithBounds TknKwWith _ }
    forall   { WithBounds TknKwForall _ }

%right '->' 
%left B
%left A
%left symbol
%right LArr

%%

Lower : lower { Name (position $1) (getData $1) }
Upper : upper { Name (position $1) (getData $1) }
Symbol : symbol { Name (position $1) (getData $1) }

Lowers : {- empty -} {[]}
       | Lowers Lower { $2 : $1 }

{- Pattern processing -}      

PatternAtoms :: { [Pattern Normal] }
          : PatternAtoms PatternAtom { $2 : $1 } 
          | PatternAtom { [$1] }

PatternAtom :: { Pattern Normal }
         : '_'             { PWild (position $1) }
         | Literal         { PLit NoExt $1 }
         | Lower           { PId NoExt $1 }
         | Upper           { PCons (getPos $1) $1 [] }
         | '(' Pattern ')' { $2 } 

PatternCons :: { Pattern Normal }
             : Upper PatternAtoms { PCons (getPos $1 <> headOr $2 getPos (getPos $1)) $1 (reverse $2)}

Pattern :: { Pattern Normal }
         : PatternCons { $1 }
         | PatternAtom { $1 }

{- Type processing -}

TypeAtomsCons :: { [Typer Normal] }
       : TypeAtoms TypeAtom { $2 : $1 }
       | TypeAtom { [$1] } 

TypeAtoms :: { [Typer Normal] }
       : TypeAtoms TypeAtom { $2 : $1 }
       | TypeAtom { [$1] } 

TypeAtomsZ :: { [Typer Normal] }
       : TypeAtomsZ TypeAtom { $2 : $1 }
       | {- empty -} { [] } 

TypeConst :: { Typer Normal } 
          : Upper TypeAtoms { 
            let pos = (headOr $2 ((getPos $1 <>) . getPos) (getPos $1)) in 
            foldl (TApp pos) (TSimple NoExt $1) (reverse $2) }

TypeAtom :: { Typer Normal }
          : Lower { TPoly NoExt $1 }
          | Upper { TSimple NoExt $1 }
          | '(' Type ')' { $2 }

TypeLvl :: { Typer Normal }
         : TypeAtom { $1 }
         | TypeConst { $1 }

TypeArrow :: { Typer Normal }
           : TypeLvl '->' TypeArrow { TArrow (getPos $1 <> getPos $3) $1 $3 }
           | TypeLvl                { $1 }

Type :: { Typer Normal }
      : TypeArrow { $1 }
      | forall Lower '.' Type { TForall (getPos $2 <> getPos $4) $2 $4 }

{- Binders -}

Binder :: { Binder Normal }
        : PatternAtom { Raw NoExt $1 }
        | '(' Pattern ':' Type ')' { Typed ($2 `mix` $4) $2 $4} 

Binders :: { [Binder Normal] }
         : Binders Binder { $2 : $1 } 
         | {- empty -} { [] }

Literal :: { Literal Normal }
         : string { LString (position $1) (getData $1) }
         | number { LInt (position $1) (getDecimal $1) }
         | double { LDouble (position $1) (getDouble $1) }
         | char   { LChar (position $1) (getLitChar $1) }

Atom :: { Expr Normal }
      : Literal      { Lit NoExt $1 }
      | Lower        { Var NoExt $1 }
      | Upper        { Var NoExt $1 }
      | '(' Expr ')' { $2 }

Call :: { Expr Normal }
      : Call Atom { App ($1 `mix` $2) $1 $2 }
      | Atom { $1 }

ExprAtom :: { Expr Normal }
          : Call { $1 }
          | ExprAtom Symbol Call { Binary ($1 `mix` $3) $2 $1 $3 }

Expr :: { Expr Normal }
      : '\\' Binder '->' Expr { Lam ($2 `mix` $4) $2 $4}
      | open Sttms Close { Block (position $1 <> (getPos . getLastSttm $ $2)) $2 }
      | match Expr with open MatchClauses Close { Match (firstAndLast $5) $2 (reverse $5) }
      | if Expr then Expr else Expr { If (position $1 <> getPos $6) $2 $4 $6 }
      | ExprAtom { $1 }

Exprs :: { [Expr Normal] }
      : Expr { [$1] } 
      | Exprs semi Expr { $3 : $1 }

MatchClause :: { (Pattern Normal, Expr Normal) }
             : Pattern '->' Expr { ($1, $3) } 

MatchClauses :: { [(Pattern Normal, Expr Normal)] }
              : MatchClause { [$1] }
              | MatchClauses semi MatchClause { $3 : $1 }

Assign : let Binder '=' Expr { Assign (position $1 <> getPos $4) $2 $4}

Sttms :: { Sttms Normal }
       : Expr { End $1 }
       | Assign {% throwError $ ERR.UnexpectedAssign $ assignPos $ $1   }
       | Expr semi Sttms   { SExpr $1 $3 } 
       | Assign semi Sttms { SAssign $1 $3 }

Close :: { () }
       : close { () }
       | error {% popLayout }

{- Type declarations -}

CoprodClause :: { (Name Normal, [Typer Normal])  }
              : '|' Upper TypeAtomsZ { ($2, reverse $3) }

CoprodClauses :: { [(Name Normal, [Typer Normal])] }
               : CoprodClause { [$1] }
               | CoprodClauses CoprodClause { $2 : $1 }

ProdClause :: { (Name Normal, Typer Normal) }
            : Lower ':' Type { ($1, $3) }

ProdClauses :: { [(Name Normal, Typer Normal)] }
             : ProdClause  { [$1] }
             | ProdClauses ',' ProdClause { $3 : $1 }

TypeCons :: { TypeCons Normal }
          : '{' ProdClauses '}' { TcRecord (position $1 <> position $3) (reverse $2) } 
          | CoprodClauses { TcSum (firstAndLast $1) (reverse $1)}
          | Type { TcSyn NoExt $1 }

TypeDecl : type Upper Lowers '=' TypeCons { TypeDecl $2 $3 $5 NoExt }

OptRet : ':' Type { Just $2 } 
       | {- empty -} { Nothing }

LetDecl : let Lower Binders OptRet '=' Expr { LetDecl $2 $3 $4 $6 NoExt }

Importable : Lower { $1 }
           | Upper { $1 }

ImportsThings : ImportsThings ',' Importable { $3 : $1 }
              | Importable { [$1] }

ModuleName : Upper { [$1] }
           | ModuleName '.' Upper { $3 : $1 }

{- 
      Program level declarations 
-}

ImportDecl : import ModuleName '(' ImportsThings ')' { ImportDecl (reverse $2) (Right $ reverse $4)  }
           | import ModuleName as Upper { ImportDecl (reverse $2) (Left $4)  }

Imports : Imports ImportDecl { $2 : $1 }
        | {- empty -} { [] }

ExternalDecl : external Lower ':' Type '=' string  
                  { ExternalDecl $2 $4 (getData $6) NoExt }

ProgramDecl : TypeDecl     { TTypeDecl $1 }
            | LetDecl      { TLetDecl $1 }
            | ExternalDecl { TExternalDecl $1 }

ProgramDecls : ProgramDecls ProgramDecl { $2 : $1 }
             | {- empty -} { [] }

Program : Imports ProgramDecls { foldl filterDecls (Program [] [] [] $1) $2 }
      
{

getLastSttm :: Sttms x -> Expr x
getLastSttm (End x) = x
getLastSttm (SAssign _ l) = getLastSttm l
getLastSttm (SExpr _ l) = getLastSttm l

filterDecls :: Program x -> TLKind x -> Program x 
filterDecls program (TTypeDecl tyDecl) = program { progType = tyDecl : progType program} 
filterDecls program (TLetDecl tyDecl) = program { progLet = tyDecl : progLet program} 
filterDecls program (TExternalDecl tyDecl) = program { progExternal = tyDecl : progExternal program} 

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
getData (WithBounds (TknSymbol tx) _)    = tx
getData (WithBounds tkn _) = error ("error while trying to get data on parser: " ++ show tkn)

getDecimal (WithBounds (TknNumber num) _) = num
getDouble (WithBounds (TknLDouble num) _) = num
getLitChar (WithBounds (TknLChar num) _) = num

-- Happy primitives

lexer = (scan >>=)
parseError = throwError . ERR.UnexpectedToken

}
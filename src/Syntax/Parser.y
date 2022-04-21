{
module Syntax.Parser where

import Syntax.Lexer.Support (Lexer, popLayout, ErrKind(..))
import Syntax.Lexer.Tokens (Token(..))
import Syntax.Lexer (scan)

import Expr
import Syntax.Tree

import Syntax.Range (Ranged(Ranged), position, Range)
import Data.Text (Text)

import Data.Function (on)
import Control.Monad.Except (throwError)

import qualified Error.Message as ERR
import Debug.Trace

}

%name parseProgram Program
%name parseExpr    Expr
%name parseType    Type

%tokentype { Ranged Token }
%monad { Lexer }
%lexer { lexer } { Ranged TknEOF _ }

%error { parseError }

%token
    lower  { Ranged (TknLowerId _) _ }
    upper  { Ranged (TknUpperId _) _ }
    hole   { Ranged (TknHole _) _ }
    symbol { Ranged (TknSymbol _) _ }

    number { Ranged (TknNumber _) _ }
    string { Ranged (TknLStr _) _ }
    char   { Ranged (TknLChar _) _ }
    double { Ranged (TknLDouble _) _ }

    open  { Ranged TknOpen _ }
    close { Ranged TknClose _ }
    semi  { Ranged TknEnd _ }

    '_'   { Ranged TknWild _ }
    '('   { Ranged TknLPar _ }
    ')'   { Ranged TknRPar _ }
    '{'   { Ranged TknLBrace _ }
    '}'   { Ranged TknRBrace _ }
    '='   { Ranged TknEq _ }
    ':'   { Ranged TknColon _ }
    '|'   { Ranged TknPipe _ }
    '->'  { Ranged TknRArrow _ }
    '\\'  { Ranged TknSlash _ }
    ','   { Ranged TknComma _ }
    '.'   { Ranged TknDot _ }

    type     { Ranged TknKwType _ }
    let      { Ranged TknKwLet _ }
    import   { Ranged TknKwImport _ }
    as       { Ranged TknKwAs _ }
    external { Ranged TknKwExternal _ }
    if       { Ranged TknKwIf _ }
    then     { Ranged TknKwThen _ }
    else     { Ranged TknKwElse _ }
    match    { Ranged TknKwMatch _ }
    with     { Ranged TknKwWith _ }
    forall   { Ranged TknKwForall _ }

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
      | '(' Pattern ':' Type ')' { PAnn (position $1 <> position $5) $2 $4 }
      | '(' Pattern ')' { $2 }

PatternCons :: { Pattern Normal }
      : Upper PatternAtoms { PCons (getPos $1 <> headOr $2 getPos (getPos $1)) $1 (reverse $2)}

Pattern :: { Pattern Normal }
      : PatternCons { $1 }
      | PatternAtom { $1 }


Binder :: { (Pattern Normal, Typer Normal) }
       : '(' PatternAtom ':' Type ')' { ($2, $4) }

Binders :: { [(Pattern Normal, Typer Normal)] }
      : Binder Binders { $1 : $2 }
      | {- empty lol -} { [] }

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
      : TypeAtoms {
            let rev = reverse $1 in
            let pos = (getPos (head rev) <> getPos (last rev)) in
            foldl (TApp pos) (head rev) (tail rev)
        }

TypeAtom :: { Typer Normal }
          : Lower { TPoly NoExt $1 }
          | Upper { TSimple NoExt $1 }
          | '(' Type ')' { $2 }

TypeLvl :: { Typer Normal }
         : TypeConst { $1 }

TypeArrow :: { Typer Normal }
           : TypeLvl '->' TypeArrow { TArrow (getPos $1 <> getPos $3) $1 $3 }
           | TypeLvl                { $1 }

Type :: { Typer Normal }
      : TypeArrow { $1 }
      | forall Lower '.' Type { TForall (position $1 <> getPos $4) $2 $4 }

Literal :: { Literal Normal }
         : string { LString (position $1) (getData $1) }
         | number { LInt (position $1) (getDecimal $1) }
         | double { LDouble (position $1) (getDouble $1) }
         | char   { LChar (position $1) (getLitChar $1) }

Atom :: { Expr Normal }
      : Literal      { Lit NoExt $1 }
      | hole         { EHole (position $1) (getData $1)  }
      | Lower        { Var NoExt $1 }
      | Upper        { Cons NoExt $1 }
      | '(' Expr ')' { $2 }
      | Atom '.' Lower { PostField (getPos $1 <> getPos $3) $1 $3 }
      | Atom '.' Upper { PostField (getPos $1 <> getPos $3) $1 $3 } -- Fix it later.

Call :: { Expr Normal }
      : Call Atom { App ($1 `mix` $2) $1 $2 }
      | Atom { $1 }

ExprAtom :: { Expr Normal }
      : Call { $1 }
      | ExprAtom Symbol Call { Binary ($1 `mix` $3) $2 $1 $3 }

NanoExpr :: { Expr Normal }
      : '\\' PatternAtom '->' NanoExpr { Lam ($2 `mix` $4) $2 $4}
      | open Sttms Close { Block (position $1 <> (getPos . getLastSttm $ $2)) $2 }
      | match NanoExpr with open MatchClauses Close { Match (firstAndLast $5) $2 (reverse $5) }
      | if NanoExpr then NanoExpr else NanoExpr { If (position $1 <> getPos $6) $2 $4 $6 }
      | ExprAtom { $1 }

Expr :: { Expr Normal }
      : NanoExpr OptRet {
            case $2 of
                  Just r -> Ann (getPos $1 <> getPos r) $1 r
                  Nothing -> $1
        }

Exprs :: { [Expr Normal] }
      : Expr { [$1] }
      | Exprs semi Expr { $3 : $1 }

MatchClause :: { (Pattern Normal, Expr Normal) }
             : PatternAtom '->' Expr { ($1, $3) }

MatchClauses :: { [(Pattern Normal, Expr Normal)] }
              : MatchClause { [$1] }
              | MatchClauses semi MatchClause { $3 : $1 }

Assign : let Pattern OptRet '=' Expr { Assign (position $1 <> getPos $5) $2 $3 $5}

Sttms :: { Sttms Normal }
       : Expr { End $1 }
       | Assign {% throwError $ UnexpectedAssign $ assignPos $ $1   }
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

LetDecl : let Lower Binders ':' Type '=' Expr { LetDecl $2 $3 $5 $7 (position $1 <> getPos $7) }

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

firstAndLast :: (HasPosition a, HasPosition b) => [(a, b)] -> Range
firstAndLast [(a,b)] = a `mix` b
firstAndLast [] = error "zero clauses!"
firstAndLast other =
      case (head other, last other) of
            ((_, b), (a, _)) -> a `mix` b

headOr :: [a] -> (a -> b) -> b -> b
headOr [] fn alt       = alt
headOr (x : xn) fn alt = fn x

mix :: (HasPosition a, HasPosition b) => a -> b -> Range
mix a b = getPos (a,b)

getData :: Ranged Token -> Text
getData (Ranged (TknLowerId tx) _) = tx
getData (Ranged (TknUpperId tx) _) = tx
getData (Ranged (TknLStr tx) _)    = tx
getData (Ranged (TknSymbol tx) _)    = tx
getData (Ranged (TknHole tx) _)    = tx
getData (Ranged tkn _) = error ("error while trying to get data on parser: " ++ show tkn)

getDecimal (Ranged (TknNumber num) _) = num
getDouble (Ranged (TknLDouble num) _) = num
getLitChar (Ranged (TknLChar num) _) = num

-- Happy primitives

lexer = (scan >>=)
parseError = throwError . UnexpectedToken

}
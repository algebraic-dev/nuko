{
module Syntax.Parser where

import Syntax.Lexer.Support (Token(..), Lexer)
import Syntax.Lexer (scan)

import Control.Monad.Except
import Data.Text (Text, append)
import Syntax.Bounds
import Data.Function (on)

import Syntax.Parser.Expr
import Data.Text.Read

import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE

import Data.Function (on)

}

%name parseDecls Decls

%tokentype { WithBounds Token }
%monad { Lexer }
%lexer { lexer } { WithBounds TknEOF _ }

%errorhandlertype explist
%error { parseError }

%token
  lower   { WithBounds (TknLowerId _) _ } 
  upper   { WithBounds (TknUpperId _) _ }
  str     { WithBounds (TknLStr _) _    }
  num     { WithBounds (TknNumber _) _    }

  let     { WithBounds TknKwLet _         }
  type    { WithBounds TknKwType _        }
  do      { WithBounds TknKwDo _          }
  case    { WithBounds TknKwCase _        }
  of      { WithBounds TknKwOf _          }
  if      { WithBounds TknKwIf _          }
  then    { WithBounds TknKwThen _        }
  else    { WithBounds TknKwElse _        }

  '='     { WithBounds TknEq _            }
  '('     { WithBounds TknLPar _          }
  ')'     { WithBounds TknRPar _          }
  '{'     { WithBounds TknLBrace _       }
  '}'     { WithBounds TknRBrace _       }

  open    { WithBounds TknOpen _          }
  end     { WithBounds TknEnd _           }
  close   { WithBounds TknClose _         }

  '->'    { WithBounds TknRArrow _  }
  '\\'    { WithBounds TknSlash _  }
  '|'     { WithBounds TknPipe _  }
  ':'     { WithBounds TknColon _  }
  ','     { WithBounds TknComma _  }
  
  '+'     { WithBounds (TknSymbol "+") _  }
  '-'     { WithBounds (TknSymbol "-") _  }
  '*'     { WithBounds (TknSymbol "*") _  }
  '/'     { WithBounds (TknSymbol "/") _  }
  '>'     { WithBounds (TknSymbol ">") _  }
  '<'     { WithBounds (TknSymbol "<") _  }
  '>='    { WithBounds (TknSymbol ">=") _ }
  '<='    { WithBounds (TknSymbol "<=") _ }
  '=='    { WithBounds (TknSymbol "==") _ }

%right Arrow

%left '+' '-'
%left '/' '*'
%nonassoc '>' '<' '>=' '<=' '=='

%%

LowerId : lower { WithBounds (getText $1) (bounds $1) }
UpperId : upper { WithBounds (getText $1) (bounds $1) }

Lit : str { LString (bounds $1) (getText $1) }
    | num { LNumber (bounds $1) (unpack . decimal . getText $ $1)}
    
RawType : LowerId        { TGen $1 }
        | UpperId        { TSimple $1 }
        | '(' Type ')'   { $2 }

Types : {- empty -}   { [] } 
      | Types RawType { $2 : $1 }

Type : RawType                       { $1 } 
     | UpperId RawType Types         { TCons (mixNonEmpty (TSimple $1 :| $2 : $3)) $1 ($2 :| $3) }
     | Type '->' RawType %prec Arrow { TArrow (mixBounds (getPos $1) (getPos $3)) $1 $3 }

Pat : LowerId    { PId $1 }
     | Lit         { PLit $1 }
     | '(' Pat ')' { $2 }

Ann : Pat                  { Raw $1 }
    | '(' Pat ':' Type ')' { Ann (mixBounds (getPos $1) (getPos $4)) $2 $4 }

Atom : Lit           { ELit $1 }
     | LowerId       { EId $1 }
     | UpperId       { EId $1 }
     | '(' Expr ')'  { $2 }
     | Atom '+' Atom  { EBinary (mixPos $1 $3) (symbolToId $2) $1 $3 }
     | Atom '-' Atom  { EBinary (mixPos $1 $3) (symbolToId $2) $1 $3 }
     | Atom '*' Atom  { EBinary (mixPos $1 $3) (symbolToId $2) $1 $3 }
     | Atom '/' Atom  { EBinary (mixPos $1 $3) (symbolToId $2) $1 $3 }
     | Atom '>' Atom  { EBinary (mixPos $1 $3) (symbolToId $2) $1 $3 }
     | Atom '<' Atom  { EBinary (mixPos $1 $3) (symbolToId $2) $1 $3 }
     | Atom '>=' Atom { EBinary (mixPos $1 $3) (symbolToId $2) $1 $3 }
     | Atom '<=' Atom { EBinary (mixPos $1 $3) (symbolToId $2) $1 $3 }
     | Atom '==' Atom { EBinary (mixPos $1 $3) (symbolToId $2) $1 $3 }

Assign : Ann '=' Expr { EAssign (mixBounds (getPos $1) (getPos $3)) $1 $3 }
        
Expr : '\\' Ann '->' Expr %prec Arrow { ELambda (mixBounds (getPos $1) (getPos $4)) $2 $4 }
     | Call                           { $1 }
     | do open Sttms close            { EBlock (mixBounds (getPos $1) (headOr $3 (getPos $1))) (reverse $3)}
     | Assign                         { $1 }
     | Case                           { $1 }


Call : Call Atom { ECall (mixPos $1 $2) $1 $2 }
     | Atom      { $1 }
     

Sttms : {- empty -}    { [] }
      | Expr           { [$1] }
      | Expr end Sttms { $1 : $3 }

Args : {- empty -} { [] }
     | Args Ann { $2 : $1 }


CaseClause :: { (Pat, Expr) }
CaseClause : Pat '->' Expr { ($1, $3) }

CaseClauses :: { [(Pat, Expr)] }
            : CaseClause { [$1] }
            | CaseClauses end CaseClause { $3 : $1 }

Case : case Expr of open CaseClauses close { ECase (mixPos $2 (snd $ last $5)) $2 $5 }

-- Type declarations 

TypesGen : {- empty -} { [] }
         | LowerId TypesGen { $1 : $2 }

SumClause : '|' UpperId Types { ($2, reverse $3) }
ProductClause : LowerId ':' Type { ($1, $3) }

SumClauses : {- empty -} { [] }
           | SumClauses SumClause { $2 : $1 }

ProductClauses : ProductClause  { [$1] }
               | ProductClauses ',' ProductClause { $3 : $1 }

TypeDeclBody  : SumClause SumClauses { TDSumType ($1 :| reverse $2) }
              | '{' ProductClauses '}' { TDProductType (reverse $2) }
              | '{' '}' { TDProductType [] }
              | Type { TDSynonym $1 }

-- Declarations 

OptRet : {- empty -} { Nothing }
       | ':' Type { Just $2 }

Decl : type UpperId TypesGen '=' TypeDeclBody { DType $2 $3 $5 }  
     | let LowerId Args OptRet '=' Expr { DLet $2 (reverse $3) $6 $4 }

Decls : {- empty -} {[]}
      | Decl Decls { $1 : $2 }

{

-- Partial functions that i use because

symbolToId (WithBounds (TknSymbol n) b) = WithBounds n b

headOr :: HasPosition a => [a] -> Bounds -> Bounds
headOr [] other   = other 
headOr (x : xs) _ = getPos x

mixNEBounds ne = mixBounds (NE.head ne) (NE.last ne)

mixPos :: HasPosition a => a -> a -> Bounds
mixPos = mixBounds `on` getPos

mixNonEmpty ne = mixBounds (getPos $ NE.head ne) (getPos $ NE.last ne)

unpack (Right (r, _)) = r

getText :: WithBounds Token -> Text
getText (WithBounds (TknLowerId tx) _) = tx
getText (WithBounds (TknUpperId tx) _) = tx
getText (WithBounds (TknLStr tx) _)    = tx
getText (WithBounds (TknNumber tx) _) = tx

-- Happy primitives

lexer = (scan >>=)
parseError = throwError . show 

}
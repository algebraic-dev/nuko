module Nuko.Syntax.Lexer.Tokens (
    Token(..),
) where

import Relude

import Pretty.Tree (PrettyTree)

-- | Unit of information that the lexer will use
-- it does not stores any metadata like position.
data Token
  -- Keywords
  = TcLet          -- | The "let" keyword
  | TcMatch        -- | The "match" keyword
  | TcWith         -- | The "with" keyword
  | TcIf           -- | The "if" keyword
  | TcThen         -- | The "then" keyword
  | TcImport       -- | The "import" keyword
  | TcAs           -- | The "as" keyword
  | TcElse         -- | The "else" keyword
  | TcType         -- | The "type" keyword
  | TcForall       -- | The "forall" keyword
  | TcPub          -- | The "public" keyword
  | TcDo           -- | The "do" keyword

  | TcStr Text     -- | String literal
  | TcInt Int      -- | Integer literal

  | TcLBracket     -- | {
  | TcRBracket     -- | }
  | TcLBrace       -- | [
  | TcRBrace       -- | ]
  | TcArrow        -- | ->
  | TcDoubleArrow  -- | \=>
  | TcLowerId Text -- | lowerId
  | TcUpperId Text -- | GreaterId
  | TcLPar         -- | (
  | TcRPar         -- | )
  | TcColon        -- | :
  | TcEqual        -- | =
  | TcPipe         -- | |
  | TcComma        -- | ,
  | TcSlash        -- | \\
  | TcDot          -- | .
  | TcWild         -- | _

  -- Virtual tokens
  | TcBegin        -- The beginning of an identation block
  | TcSep          -- Separator between two expressions in an indentation block
  | TcEnd          -- The end of an indentation block
  | TcEOF
  deriving (Show, Generic)

instance PrettyTree Token where

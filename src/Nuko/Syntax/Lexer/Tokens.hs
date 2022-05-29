module Nuko.Syntax.Lexer.Tokens (
    Token(..)
) where

import Data.Text (Text)

import qualified Data.Text as Text

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
  | TcOpen         -- | The "open" keyword
  | TcType         -- | The "type" keyword
  | TcForall       -- | The "public" keyword
  | TcPub          -- | The "public" keyword

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

instance Show Token where
  show = \case
    TcLet      -> "let"
    TcMatch    -> "match"
    TcAs       -> "as"
    TcImport   -> "import"
    TcWith     -> "with"
    TcForall   -> "forall"
    TcIf       -> "if"
    TcThen     -> "then"
    TcElse     -> "else"
    TcType     -> "type"
    TcPub      -> "pub"
    TcStr s    -> show s
    TcInt i    -> "$int(" ++ show i ++ ")"
    TcLBracket -> "'{'"
    TcRBracket -> "'}'"
    TcWild     -> "_"
    TcLBrace   -> "'['"
    TcRBrace   -> "']'"
    TcArrow    -> "->"
    TcDoubleArrow -> "=>"
    TcLowerId s -> "lower(" ++ Text.unpack s ++ ")"
    TcUpperId s -> "upper(" ++ Text.unpack s ++ ")"
    TcLPar  -> "'('"
    TcRPar  -> "')'"
    TcColon -> "':'"
    TcEqual -> "'='"
    TcPipe  -> "'|'"
    TcComma -> "','"
    TcSlash -> "'\\'"
    TcDot   -> "'.'"
    TcBegin -> "$begin"
    TcSep   -> "$semi"
    TcEnd   -> "$end"
    TcEOF   -> "$EOF"
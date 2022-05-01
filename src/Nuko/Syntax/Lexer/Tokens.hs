module Nuko.Syntax.Lexer.Tokens (
    Token(..)
) where

import Data.Text (Text)
import Data.Int  (Int64)

import qualified Data.Text as Text

-- | Unit of information that the lexer will use
-- it does not stores any metadata like position.
data Token
    -- Keywords
    = TcLet          -- The "let" keyword
    | TcCase         -- The "case" keyword
    | TcMatch        -- The "match" keyword
    | TcOf           -- The "of" keyword
    | TcIf           -- The "if" keyword
    | TcThen         -- The "then" keyword
    | TcElse         -- The "else" keyword
    | TcType         -- The "type" keyword
    | TcPub          -- The "public" keyword

    | TcStr Text     -- String literal
    | TcInt Int64    -- Integer literal

    | TcLBracket     -- {
    | TcRBracket     -- }
    | TcLBrace       -- [
    | TcRBrace       -- ]

    | TcArrow        -- ->
    | TcDoubleArrow  -- \=>
    | TcLowerId Text -- lowerId
    | TcUpperId Text -- GreaterId
    | TcLPar         -- (
    | TcRPar         -- )
    | TcColon        -- :
    | TcEqual        -- =
    | TcSlash        -- /
    | TcDot          -- ^.

    -- Virtual tokens
    | TcBegin        -- The beginning of an identation block
    | TcSep          -- Separator between two expressions in an indentation block
    | TcEnd          -- The end of an indentation block
    | TcEOF

instance Show Token where
    show = \case
        TcLet   -> "let"
        TcCase  -> "case"
        TcMatch -> "match"
        TcOf    -> "of"
        TcIf    -> "if"
        TcThen  -> "then"
        TcElse  -> "else"
        TcType  -> "type"
        TcPub   -> "pub"
        TcStr s -> show s
        TcInt i -> "int(" ++ show i ++ ")"
        TcLBracket -> "{"
        TcRBracket -> "}"
        TcLBrace -> "["
        TcRBrace -> "]"
        TcArrow -> "->"
        TcDoubleArrow -> "=>"
        TcLowerId s -> Text.unpack s
        TcUpperId s -> Text.unpack s
        TcLPar  -> "("
        TcRPar  -> ")"
        TcColon -> ":"
        TcEqual -> "="
        TcSlash -> "\\"
        TcDot   -> "."
        TcBegin -> "$begin"
        TcSep   -> "$semi"
        TcEnd   -> "$end"
        TcEOF   -> "$EOF"
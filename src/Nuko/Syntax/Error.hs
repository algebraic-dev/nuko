module Nuko.Syntax.Error (SyntaxError(..)) where

import Relude (Show)
import Nuko.Syntax.Range ( Ranged, Range, Pos )
import Nuko.Syntax.Lexer.Tokens (Token)

data SyntaxError
  = UnexpectedChar Pos
  | UnexpectedSeq Range
  | UnfinishedStr Pos
  | UnexpectedToken (Ranged Token)
  | CannotAssign Range
  deriving Show
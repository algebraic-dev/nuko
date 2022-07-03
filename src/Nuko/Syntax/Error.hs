module Nuko.Syntax.Error (
  SyntaxError(..),
  Case(..),
) where

import Relude (Show)
import Nuko.Syntax.Range ( Ranged, Range, Pos )
import Nuko.Syntax.Lexer.Tokens (Token)

data Case = UpperCase | LowerCase
  deriving Show

data SyntaxError
  = UnexpectedStr Range
  | UnfinishedStr Pos
  | UnexpectedToken (Ranged Token)
  | AssignInEndOfBlock Range
  | WrongUsageOfCase Case Range
  deriving Show
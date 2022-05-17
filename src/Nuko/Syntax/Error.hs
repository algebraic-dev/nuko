module Nuko.Syntax.Error (
    LexingError(..)
) where

import Nuko.Error.Data (CompilerError(..), Colored(Normal), posErr, rangeErr)
import Nuko.Syntax.Range (Pos, Ranged (..))
import Nuko.Syntax.Lexer.Tokens (Token)

data LexingError
    = UnexpectedChar Pos
    | UnfinishedStr Pos
    | UnexpectedToken (Ranged Token)
    deriving Show

instance CompilerError LexingError where
  getReport report = \case
    (UnexpectedChar point) ->
      posErr report point
        [Normal "Cannot understand the char."]
    (UnfinishedStr range) ->
      posErr report range
        [Normal "You have not finished the string."]
    (UnexpectedToken tkn) ->
      rangeErr report tkn.position
        [Normal "Unexpected token!"]
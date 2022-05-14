module Nuko.Syntax.Error (
    LexingError(..)
) where

import Nuko.Error.Data (CompilerError(..), Colored(Normal), onePointErr, rangeErr)
import Nuko.Syntax.Range (Point, Ranged (..))
import Nuko.Syntax.Lexer.Tokens (Token)

data LexingError
    = UnexpectedChar Point
    | UnfinishedStr Point
    | UnexpectedToken (Ranged Token)
    deriving Show

instance CompilerError LexingError where
  getReport report = \case
    (UnexpectedChar point) ->
      onePointErr report point
        [Normal "Cannot understand the char."]
    (UnfinishedStr range) ->
      onePointErr report range
        [Normal "You have not finished the string."]
    (UnexpectedToken tkn) ->
      rangeErr report tkn.position
        [Normal "Unexpected token!"]
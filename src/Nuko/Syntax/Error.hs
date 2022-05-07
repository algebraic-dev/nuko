module Nuko.Syntax.Error (
    LexingError(..)
) where

import Nuko.Error.Data
import Nuko.Syntax.Range (Point)

data LexingError
    = UnexpectedChar Point
    | UnfinishedStr Point
    deriving Show

instance CompilerError LexingError where
  getReport report = \case
    (UnexpectedChar point) ->
      onePointErr report point
        [Normal "Cannot understand the char."]
    (UnfinishedStr range) ->
      onePointErr report range
        [Normal "You have not finished the string."]
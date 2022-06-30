module Nuko.Resolver.Error (ResolveError(..), Case(..)) where

import Relude            (Show, Text, NonEmpty)
import Nuko.Syntax.Range (Range(..))
import Nuko.Syntax.Tree  (Name)

data Case = UpperCase | LowerCase
  deriving Show

data ResolveError
  = CyclicImport Range
  | CannotFindModule Text Range
  | CannotFindFunction Text Text Range
  | CannotFindType Text Text Range
  | CannotFindConstructor Text Text Range
  | IsPrivate Text Range
  | AmbiguousNames (NonEmpty Text)
  deriving Show
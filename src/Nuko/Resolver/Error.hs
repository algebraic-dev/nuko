module Nuko.Resolver.Error (ResolveError(..), Case(..)) where

import Relude            (Show, Text, HashSet)
import Nuko.Syntax.Range (Range(..))

data Case = UpperCase | LowerCase
  deriving Show

data ResolveError
  = CyclicImport Range
  | CannotFindModule Text Range
  | CannotFindFunction Text Text Range
  | CannotFindType Text Text Range
  | CannotFindConstructor Text Text Range
  | IsPrivate Text Range
  | AmbiguousNames (HashSet Text)
  deriving Show
module Nuko.Resolver.Error (
  ResolveError(..),
  Case(..),
) where

import Relude                   (Show, Text, HashSet, Maybe)
import Nuko.Syntax.Range        (Range(..))
import Nuko.Resolver.Occourence (NameKind)
import Data.List.NonEmpty       (NonEmpty)

data Case = UpperCase | LowerCase
  deriving Show

data ResolveError
  = CyclicImport Text Range
  | CannotFindModule Text Range
  | CannotFindInModule (NonEmpty NameKind) (Maybe Text) Text Range
  | IsPrivate NameKind Text Range
  | AmbiguousNames (HashSet (Text, Text))
  | AlreadyExistsName Text NameKind Range
  | AlreadyExistsPat Text Range
  deriving Show
module Nuko.Resolver.Error (
  ResolveError(..),
  Case(..),
) where

import Relude                    (Show, Text, HashSet, Maybe)
import Relude.List.NonEmpty      (NonEmpty)
import Nuko.Report.Range         (Range(..))
import Nuko.Resolver.Occourence  (NameKind)
import Nuko.Resolver.Environment (Qualified)

data Case = UpperCase | LowerCase
  deriving Show

data ResolveError
  = CyclicImport Text Range
  | CannotFindModule Text Range
  | CannotFindInModule (NonEmpty NameKind) (Maybe Text) Text Range
  | IsPrivate NameKind Text Range
  | AmbiguousNames (HashSet Qualified)
  | AlreadyExistsName Text NameKind Range
  | ConflictingTypes (NonEmpty (Text, Range))
  | AlreadyExistsPat Text Range
  deriving Show
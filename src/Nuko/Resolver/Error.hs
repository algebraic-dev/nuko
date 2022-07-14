module Nuko.Resolver.Error (
  ResolveError(..),
  Case(..),
) where

import Relude                    (Show, Text, HashSet, Maybe)
import Relude.List.NonEmpty      (NonEmpty)
import Nuko.Report.Range         (Range(..))
import Nuko.Names                (NameSort, Path, Label, Ident, Qualified, ModName, ValName, Name, TyName)
import GHC.Generics (Generic)
import Pretty.Tree (PrettyTree)

data Case = UpperCase | LowerCase
  deriving Show

data ResolveError
  = CyclicImport ModName
  | CannotFindModule ModName
  | CannotFindInModule (NonEmpty NameSort) (Maybe ModName) Ident
  | IsPrivate (Path Label)
  | AmbiguousNames (HashSet (Qualified Label))
  | AlreadyExistsName Label
  | ConflictingTypes (NonEmpty (Name TyName))
  | AlreadyExistsPat Label
  deriving Generic

instance PrettyTree ResolveError where
module Nuko.Resolver.Error (
  ResolveErrorReason(..),
  Case(..)
) where

import Relude                    (Show, HashSet, Int, head)
import Relude.List.NonEmpty      (NonEmpty)
import Nuko.Names                (NameSort, Path, Label, Ident, Qualified, ModName, Name, TyName, ValName)
import Data.Aeson                (ToJSON(..), KeyValue ((.=)), object)
import Pretty.Format             (Format(format), formatOr)
import Nuko.Report.Text          (Mode (..), Color (..), Piece (..), colorlessFromFormat)
import Nuko.Report.Range         (Range, HasPosition (..), toLabel)

data Case = UpperCase | LowerCase
  deriving Show

data ResolveErrorReason
  = CyclicImport ModName
  | CannotFindModule ModName
  | CannotFindInModule (NonEmpty NameSort) (Path Ident)
  | IsPrivate (Path Label)
  | AmbiguousNames Range (HashSet (Qualified Label))
  | AlreadyExistsName Label
  | ConflictingTypes (NonEmpty (Name TyName))
  | AlreadyExistsPat (Name ValName)
  | ShouldAppearOnOr (Name ValName)
  | CannotIntroduceNewVariables (Name ValName)

errorCode :: ResolveErrorReason -> Int
errorCode = \case
  CyclicImport {} -> 100
  CannotFindModule {} -> 101
  CannotFindInModule {} -> 102
  IsPrivate {} -> 103
  AmbiguousNames {} -> 104
  AlreadyExistsName {} -> 105
  ConflictingTypes {} -> 106
  AlreadyExistsPat {} -> 107
  ShouldAppearOnOr {} -> 108
  CannotIntroduceNewVariables {} -> 109

errorTitle :: ResolveErrorReason -> Mode
errorTitle = \case
  CyclicImport mod ->
    Words [Raw "Cyclic import in module", Marked Fst (format mod)]
  CannotFindModule mod ->
    Words [Raw "Cannot find module", Marked Fst (format mod)]
  CannotFindInModule sorts path ->
    Words [Raw "Cannot find", Raw (formatOr sorts), Raw "from", Marked Fst (format path) ]
  IsPrivate path ->
    Words [Raw "You cannot use", Marked Fst (format path), Raw "because it's private!"]
  AmbiguousNames _ _ ->
    Words [Raw "Cannot decide what import to use"]
  AlreadyExistsName label ->
    Words [Raw "The name", Marked Fst (format label), Raw "is already defined in the same module!"]
  ConflictingTypes typ ->
     Words [Raw "You cannot use the type variable name", Marked Fst (format (head typ)) ,Raw "twice"]
  AlreadyExistsPat pat ->
    Words [Raw "You cannot use the name", Marked Fst (format pat) ,Raw "twice in a pattern"]
  ShouldAppearOnOr name ->
    Words [Raw "The pattern", Marked Fst (format name), Raw "should appear on each side of the or pattern"]
  CannotIntroduceNewVariables name ->
    Words [Raw "Cannot introduce the variable", Marked Fst (format name), Raw "in the left side of a Or Pattern"]

getErrorSite :: ResolveErrorReason -> Range
getErrorSite = \case
  CyclicImport mod -> getPos mod
  CannotFindModule mod -> getPos mod
  CannotFindInModule _ path -> getPos path
  IsPrivate path -> getPos path
  AmbiguousNames range _ -> range
  AlreadyExistsName path -> getPos path
  ConflictingTypes path -> getPos (head path)
  AlreadyExistsPat path -> getPos path
  ShouldAppearOnOr path -> getPos path
  CannotIntroduceNewVariables path -> getPos path

instance ToJSON ResolveErrorReason where
  toJSON reason =
    object [ "code" .= errorCode reason
           , "place" .= toLabel (getErrorSite reason)
           , "title" .= colorlessFromFormat (errorTitle reason)
           ]
module Nuko.Typer.Error (
  TypeError(..),
) where

import Relude           (Int)
import Nuko.Typer.Types (TTy, Relation (..), TKind)
import Nuko.Names       (Label(..))
import Nuko.Report.Text (Mode (..), Piece (..), colorlessFromFormat)
import Data.Aeson.Types (ToJSON(toJSON), (.=), object, Value)
import Pretty.Format (format)

data TypeError
  = Mismatch (TTy 'Virtual) (TTy 'Virtual)
  | KindMismatch TKind TKind
  | OccoursCheck (TTy 'Virtual) (TTy 'Virtual)
  | OccoursCheckKind TKind TKind
  | EscapingScope
  | NotAFunction (TTy 'Virtual)
  | NameResolution Label
  | CyclicTypeDef [Label]
  | ExpectedConst Int Int
  | CannotInferField
  | NotExhaustive

errorTitle :: TypeError -> Mode
errorTitle = \case
  Mismatch {} -> Words [Raw "Type Mismatch"]
  KindMismatch {} -> Words [Raw "Kind Mismatch"]
  OccoursCheck {} -> Words [Raw "Infinite Type"]
  OccoursCheckKind {} -> Words [Raw "Infinite Kind"]
  EscapingScope {} -> Words [Raw "Escaping Scope"]
  NotAFunction t -> Words [Raw "The type", Raw (format t) , Raw " is not a function type"]
  NameResolution label -> Words [Raw "Compiler bug: Error resolution incomplete for", Raw (format label)]
  CyclicTypeDef {} -> Words [Raw "Cyclic type"]
  ExpectedConst expected got -> Words [Raw "Expected", Raw (format expected), Raw "arguments for the type constructor but got", Raw (format got)]
  CannotInferField -> Words [Raw "Cannot infer field for type"]
  NotExhaustive -> Words [Raw "Patterns are not exhaustive"]

getDetails :: TypeError -> Value
getDetails = \case
  Mismatch expected got -> object ["expected" .= format expected, "got" .= format got]
  KindMismatch expected got -> object ["expected" .= format expected, "got" .= format got]
  OccoursCheck fst snd -> object ["fst" .= format fst, "snd" .= format snd]
  OccoursCheckKind fst snd -> object ["fst" .= format fst, "snd" .= format snd]
  EscapingScope {} -> object []
  NotAFunction _ -> object []
  NameResolution {} -> object []
  CyclicTypeDef {} -> object []
  ExpectedConst {} -> object []
  CannotInferField -> object []
  NotExhaustive -> object []

instance ToJSON TypeError where
  toJSON reason =
    object [ "title" .= colorlessFromFormat (errorTitle reason)
           , "details" .= getDetails reason
           ]
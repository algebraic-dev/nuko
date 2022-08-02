module Nuko.Typer.Error (
  TypeError(..),
) where

import Relude

import Data.Aeson.Types  (ToJSON (toJSON), Value, object, (.=))
import Nuko.Names        (Label (..))
import Nuko.Report.Range (Range)
import Nuko.Report.Text
import Nuko.Typer.Types  (Relation (..), TKind, TTy)
import Pretty.Format     (format, formatAnd)

data TypeError
  = Mismatch Range (TTy 'Virtual) (TTy 'Virtual)
  | KindMismatch Range TKind TKind
  | OccoursCheck Range (TTy 'Virtual) (TTy 'Virtual)
  | OccoursCheckKind Range TKind TKind
  | EscapingScope Range
  | NotAFunction Range (TTy 'Virtual)
  | NameResolution Range Label
  | CyclicTypeDef Range [Label]
  | ExpectedConst Range Int Int
  | CannotInferField Range
  | NotExhaustive Range (NonEmpty Text)
  | UselessClause Range

errorTitle :: TypeError -> Mode
errorTitle = \case
  Mismatch {} -> Words [Raw "Type Mismatch"]
  KindMismatch {} -> Words [Raw "Kind Mismatch"]
  OccoursCheck {} -> Words [Raw "Infinite Type"]
  OccoursCheckKind {} -> Words [Raw "Infinite Kind"]
  EscapingScope {} -> Words [Raw "Escaping Scope"]
  NotAFunction _ t -> Words [Raw "The type", Raw (format t) , Raw " is not a function type"]
  NameResolution _ label -> Words [Raw "Compiler bug: Error resolution incomplete for", Raw (format label)]
  CyclicTypeDef {} -> Words [Raw "Cyclic type"]
  ExpectedConst _ expected got -> Words [Raw "Expected", Raw (format expected), Raw "arguments for the type constructor but got", Raw (format got)]
  CannotInferField {} -> Words [Raw "Cannot infer field for type"]
  NotExhaustive {}  -> Words [Raw "Patterns are not exhaustive"]
  UselessClause {}  -> Words [Raw "The pattern is useless is useless for the rest of the match!"]

getDetails :: TypeError -> Value
getDetails = \case
  Mismatch _ expected got -> object ["expected" .= format expected, "got" .= format got]
  KindMismatch _ expected got -> object ["expected" .= format expected, "got" .= format got]
  OccoursCheck _ fst snd -> object ["fst" .= format fst, "snd" .= format snd]
  OccoursCheckKind _ fst snd -> object ["fst" .= format fst, "snd" .= format snd]
  EscapingScope {} -> object []
  NotAFunction _ _ -> object []
  NameResolution {} -> object []
  CyclicTypeDef {} -> object []
  ExpectedConst {} -> object []
  CannotInferField {} -> object []
  NotExhaustive {} -> object []
  UselessClause {} -> object []

instance PrettyDiagnostic TypeError where
  prettyDiagnostic err = case err of
    Mismatch r got expected ->
      DetailedDiagnosticInfo
        (Words [Raw "Type mismatch"])
        [(Fst, Words [Raw "Expected:", Marked Fst (format expected)])
        ,(Snd, Words [Raw "     Got:", Marked Snd (format got)])]
        []
        [Ann Fst (Words [Raw (format got)]) r]
    KindMismatch r got expected ->
      DetailedDiagnosticInfo
        (Words [Raw "Kind mismatch"])
        [(Fst, Words [Raw "Expected:", Marked Fst (format expected)])
        ,(Snd, Words [Raw "     Got:", Marked Snd (format got)])]
        []
        [Ann Fst (Words [Raw (format got)]) r]
    OccoursCheck range _ _ ->
        mkBasicDiagnostic [Raw "This expression would cause a infinite type!"] [Ann Fst (Words [Raw "Here!"]) range]
    OccoursCheckKind range _ _ ->
        mkBasicDiagnostic [Raw "This expression would cause a infinite kind!"] [Ann Fst (Words [Raw "Here!"]) range]
    EscapingScope range ->
        mkBasicDiagnostic [Raw "This type would escape its scope"] [Ann Fst (Words [Raw "Here!"]) range]
    NotAFunction range _ ->
        mkBasicDiagnostic [Raw "Not a function type to be applied"] [Ann Fst (Words [Raw "Here!"]) range]
    NameResolution range _ ->
        mkBasicDiagnostic [Raw "Compiler bug with name resolution D: please open a PR"] [Ann Fst (Words [Raw "Here!"]) range]
    CyclicTypeDef range _ ->
        mkBasicDiagnostic [Raw "Cannot make cycles with type definitions"] [Ann Fst (Words [Raw "Here!"]) range]
    ExpectedConst range e g ->
        mkBasicDiagnostic
          [Raw "This constructor expects", Marked Fst (format e), Raw "arguments but got", Marked Snd (format g)]
          [Ann Fst (Words [Raw "Here!"]) range]
    CannotInferField range ->
        mkBasicDiagnostic [Raw "Cannot discover a type that this field have!"] [Ann Fst (Words [Raw "Here!"]) range]
    NotExhaustive range like ->
        mkBasicDiagnostic [Raw "These clauses does not match", Marked Snd (formatAnd like)] [Ann Fst (Words [Raw "Here!"]) range]
    UselessClause range ->
        mkBasicDiagnostic [Raw "The clause is useless for the other ones!"] [Ann For (Words [Raw "Here!"]) range]

instance ToJSON TypeError where
  toJSON reason =
    object [ "title" .= colorlessFromFormat (errorTitle reason)
           , "details" .= getDetails reason
           ]

module Nuko.Typer.Error (
  TypeError(..),
) where

import Relude

import Nuko.Names        (Label (..), Name, ValName)
import Nuko.Report.Range (HasPosition (getPos), Range)
import Nuko.Report.Text
import Nuko.Typer.Types  (Relation (..), TKind, TTy)
import Pretty.Format     (format, formatAnd)

data TypeError
  = Mismatch Range (TTy 'Virtual) (TTy 'Virtual)
  | MismatchInApp Range Range Int (TTy 'Virtual) (TTy 'Virtual)
  | KindMismatch Range TKind TKind
  | OccoursCheck Range (TTy 'Virtual) (TTy 'Virtual)
  | OccoursCheckKind Range TKind TKind
  | EscapingScope Range
  | NotAFunction Range (TTy 'Virtual)
  | NameResolution Range Label
  | CyclicTypeDef Range [Label]
  | ExpectedConst Range Int Int
  | CannotInferField Range
  | FieldDoesNotBelongsToTheType (Name ValName)
  | DuplicatedFieldName (Name ValName)
  | NotExhaustive Range (NonEmpty Text)
  | UselessClause Range
  | NeedMoreFields (NonEmpty (Name ValName))

instance PrettyDiagnostic TypeError where
  prettyDiagnostic err = case err of
    Mismatch r got expected ->
      DetailedDiagnosticInfo
        300
        (Words [Raw "Type mismatch"])
        [(Fst, Words [Raw "Expected:", Marked Fst (format expected)])
        ,(Snd, Words [Raw "     Got:", Marked Snd (format got)])]
        []
        [Ann Fst (Words [Raw (format got)]) r]
    MismatchInApp fnRange r place got expected ->
      DetailedDiagnosticInfo
        300
        (Words [Raw "The",Raw $ format place <> "nd", Raw "argument type is incorrect"])
        [(Fst, Words [Raw "Expected:", Marked Snd (format expected)])
        ,(Snd, Words [Raw "     Got:", Marked Fst (format got)])]
        []
        [ Ann Fst (Words [Raw "The type of this is wrong."]) r
        , Ann Thr (Words [Raw "This is the function"]) fnRange]
    KindMismatch r got expected ->
      DetailedDiagnosticInfo
        301
        (Words [Raw "Kind mismatch"])
        [(Fst, Words [Raw "Expected:", Marked Fst (format expected)])
        ,(Snd, Words [Raw "     Got:", Marked Snd (format got)])]
        []
        [Ann Fst (Words [Raw (format got)]) r]
    OccoursCheck range _ _ ->
        mkBasicDiagnostic 302 [Raw "This expression would cause a infinite type!"] [Ann Fst (Words [Raw "Here!"]) range]
    OccoursCheckKind range _ _ ->
        mkBasicDiagnostic 303 [Raw "This expression would cause a infinite kind!"] [Ann Fst (Words [Raw "Here!"]) range]
    EscapingScope range ->
        mkBasicDiagnostic 304 [Raw "This type would escape its scope"] [Ann Fst (Words [Raw "Here!"]) range]
    NotAFunction range _ ->
        mkBasicDiagnostic 305 [Raw "Not a function type to be applied"] [Ann Fst (Words [Raw "Here!"]) range]
    NameResolution range _ ->
        mkBasicDiagnostic 306 [Raw "Compiler bug with name resolution D: please open a PR"] [Ann Fst (Words [Raw "Here!"]) range]
    CyclicTypeDef range _ ->
        mkBasicDiagnostic 307 [Raw "Cannot make cycles with type definitions"] [Ann Fst (Words [Raw "Here!"]) range]
    ExpectedConst range e g ->
        mkBasicDiagnostic 308
          [Raw "This constructor expects", Marked Fst (format e), Raw "arguments but got", Marked Snd (format g)]
          [Ann Fst (Words [Raw "Here!"]) range]
    CannotInferField range ->
        mkBasicDiagnostic 309 [Raw "Cannot discover a type that this field have!"] [Ann Fst (Words [Raw "Here!"]) range]
    NotExhaustive range like ->
        mkBasicDiagnostic 310 [Raw "These clauses does not match", Marked Snd (formatAnd like)] [Ann Fst (Words [Raw "Here!"]) range]
    UselessClause range ->
        mkBasicDiagnostic 311 [Raw "The clause is useless for the other ones!"] [Ann For (Words [Raw "Here!"]) range]
    FieldDoesNotBelongsToTheType name ->
        mkBasicDiagnostic 312 [Raw "The field", Marked Fst (format name), Raw "does not exists in this type!"] [Ann Fst (Words [Raw "Here!"]) (getPos name)]
    DuplicatedFieldName name ->
        mkBasicDiagnostic 313 [Raw "The field", Marked Fst (format name), Raw "is duplicated"] [Ann Fst (Words [Raw "Here!"]) (getPos name)]
    NeedMoreFields fields ->
        mkBasicDiagnostic 313 [Raw "The record needs", Marked Fst (formatAnd fields), Raw "in order to be created"] [Ann Fst (Words [Raw "Here!"]) (getPos (head fields))]

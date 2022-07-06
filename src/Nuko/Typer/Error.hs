module Nuko.Typer.Error (
  TypeError(..),
) where

import Data.Text (Text)

data TypeError
  = CannotUnify Text Text
  | OccoursCheck
  | OccoursCheckKind Text
  | EscapingScope
  | NotAFunction
  | NameResolution Text


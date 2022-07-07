module Nuko.Typer.Error (
  TypeError(..),
) where

import Data.Text (Text)
import Relude.Base (Show)
import Relude (Generic)
import Pretty.Tree (PrettyTree)

data TypeError
  = CannotUnify Text Text
  | OccoursCheck
  | OccoursCheckKind Text
  | EscapingScope
  | NotAFunction
  | NameResolution Text
  deriving (Show, Generic)

instance PrettyTree TypeError where

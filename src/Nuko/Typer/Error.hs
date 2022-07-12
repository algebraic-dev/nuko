module Nuko.Typer.Error (
  TypeError(..),
) where

import Data.Text (Text)
import Relude.Base (Show)
import Relude (Generic, Int)
import Pretty.Tree (PrettyTree)

data TypeError
  = CannotUnify Text Text
  | OccoursCheck
  | OccoursCheckKind Text
  | EscapingScope
  | NotAFunction
  | NameResolution Text
  | CyclicTypeDef [Text]
  | CannotFindTySym
  | ExpectedConst Int Int
  | CannotInferField
  deriving (Show, Generic)

instance PrettyTree TypeError where

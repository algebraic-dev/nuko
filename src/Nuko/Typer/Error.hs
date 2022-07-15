module Nuko.Typer.Error (
  TypeError(..),
) where

import Data.Text (Text)
import Relude (Int)
import Nuko.Typer.Types (TTy, Relation (..), TKind)
import Nuko.Names (Label(..))

data TypeError
  = Mismatch (TTy 'Virtual) (TTy 'Virtual)
  | KinMismatch TKind TKind
  | OccoursCheck (TTy 'Virtual) (TTy 'Virtual)
  | OccoursCheckKind TKind
  | EscapingScope
  | NotAFunction
  | NameResolution Label
  | CyclicTypeDef [Label]
  | CannotFindTySym
  | ExpectedConst Int Int
  | CannotInferField

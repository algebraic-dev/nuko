module Typer.Error
  ( TypeError (..),
  )
where

import Control.Exception (Exception)
import Data.Text (Text)
import Typer.Types (Ty)

data TypeError
  = CannotFind Text
  | CannotUnify Ty Ty
  | OccoursCheck Ty Ty
  | EscapingScope Ty
  deriving (Show)

instance Exception TypeError
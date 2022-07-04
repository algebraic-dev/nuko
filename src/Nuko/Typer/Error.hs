module Nuko.Typer.Error (
  ErrCause(..),
  TypeError(..),
) where

import Nuko.Typer.Types (TTy, Virtual)
import Text.Show (Show)

data ErrCause
  = CannotUnify
  | OccoursCheck (TTy Virtual) (TTy Virtual)
  | EscapingScope (TTy Virtual)
  | NotAFunction (TTy Virtual)
  deriving Show

data TypeError = TypeError { cause :: ErrCause }  
  deriving Show


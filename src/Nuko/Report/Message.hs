module Nuko.Report.Message (
  CompilerError(..),
  ErrorKind(..)
) where

import Nuko.Names        (ModName)
import Data.Maybe        (Maybe)
import Nuko.Report.Range (Range, toLabel)
import Nuko.Syntax.Error (SyntaxError)
import Nuko.Typer.Error  (TypeError)
import Data.Aeson        (ToJSON(toJSON), KeyValue ((.=)), object)
import Pretty.Format     (Format(format))
import Relude            ((<$>), ($))

data ErrorKind
  = SyntaxError SyntaxError
  | TypingError TypeError

data CompilerError = CompilerError
  { moduleName :: Maybe ModName
  , position   :: Maybe Range
  , kind       :: ErrorKind
  }

instance ToJSON ErrorKind where
  toJSON = \case
    SyntaxError e -> toJSON e
    TypingError e -> toJSON e

instance ToJSON CompilerError where
  toJSON err =
    object
      [ "module"   .= (toJSON $ format <$> err.moduleName)
      , "position" .= (toJSON (toLabel <$> err.position))
      , "details"  .= (toJSON err.kind)
      ]
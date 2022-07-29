module Nuko.Report.Message (
  Diagnostic(..),
  DiagnosticInfo(..),
  Severity(..),
) where

import Nuko.Names        (ModName)
import Nuko.Report.Range (Range, toLabel)
import Nuko.Syntax.Error (SyntaxError)
import Nuko.Typer.Error  (TypeError)
import Data.Aeson        (ToJSON(toJSON), KeyValue ((.=)), object)
import Pretty.Format     (Format(format))
import Relude            (($))
import Nuko.Resolver.Error (ResolveErrorReason)
import Data.Text (Text)

data DiagnosticInfo
  = SyntaxError SyntaxError
  | ResolveError ResolveErrorReason
  | TypingError TypeError

data Severity
  = Warning
  | Error
  | Information
  | Hint

data Diagnostic = Diagnostic
  { moduleName :: ModName
  , filename   :: Text
  , severity   :: Severity
  , position   :: Range
  , kind       :: DiagnosticInfo
  }

instance ToJSON DiagnosticInfo where
  toJSON = \case
    SyntaxError e -> toJSON e
    TypingError e -> toJSON e
    ResolveError e -> toJSON e

instance ToJSON Diagnostic where
  toJSON err =
    object
      [ "module"   .= (toJSON $ format err.moduleName)
      , "position" .= (toJSON (toLabel err.position))
      , "details"  .= (toJSON err.kind)
      ]
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
import Nuko.Report.Text (Severity(..), PrettyDiagnostic(..))

data DiagnosticInfo
  = SyntaxError SyntaxError
  | ResolveError ResolveErrorReason
  | TypingError TypeError

data Diagnostic = Diagnostic
  { moduleName :: ModName
  , filename   :: Text
  , severity   :: Severity
  , position   :: Range
  , kind       :: DiagnosticInfo
  }

instance PrettyDiagnostic DiagnosticInfo where
  prettyDiagnostic = \case
    SyntaxError err -> prettyDiagnostic err
    ResolveError err -> prettyDiagnostic err
    TypingError err -> prettyDiagnostic err

instance ToJSON DiagnosticInfo where
  toJSON = \case
    SyntaxError e -> toJSON e
    TypingError e -> toJSON e
    ResolveError e -> toJSON e

instance ToJSON Diagnostic where
  toJSON err =
    object
      [ "module"   .= toJSON (format err.moduleName)
      , "position" .= toJSON (toLabel err.position)
      , "details"  .= toJSON err.kind
      ]
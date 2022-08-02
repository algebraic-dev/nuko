module Nuko.Report.Message (
  Diagnostic(..),
  DiagnosticInfo(..),
  Severity(..),
) where

import Nuko.Names          (ModName)
import Nuko.Report.Range   (Range)
import Nuko.Report.Text    (PrettyDiagnostic (..), Severity (..))
import Nuko.Resolver.Error (ResolveErrorReason)
import Nuko.Syntax.Error   (SyntaxError)
import Nuko.Typer.Error    (TypeError)

import Data.Aeson          (ToJSON (toJSON))
import Data.Text           (Text)

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
    SyntaxError err  -> prettyDiagnostic err
    ResolveError err -> prettyDiagnostic err
    TypingError err  -> prettyDiagnostic err

instance ToJSON DiagnosticInfo where
  toJSON = \case
    SyntaxError e  -> toJSON e
    TypingError e  -> toJSON e
    ResolveError e -> toJSON e

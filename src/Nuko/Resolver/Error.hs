module Nuko.Resolver.Error (
    ResolutionError(..),
    ResolutionErrorKind(..),
    simpleErr
) where

import Nuko.Resolver.Support
import Nuko.Error.Data       (CompilerError(..), Colored(..), rangeErr, Marker (Main), subtitles, Report (hints))
import Nuko.Syntax.Range     (Range(..))
import Data.Text             (Text)
import Data.List.NonEmpty    (NonEmpty((:|)))
import Data.Maybe (catMaybes)

data ResolutionErrorKind
    = AmbiguousNames   Range Text Text Resolution
    | VariableNotFound Range Text
    | ConstructorNotFound Range Text
    | TypeNotFound Range Text
    | ModuleNotFound   Range Text
    | DuplicatedPatId Range Text

data ResolutionError = ResolutionError
  { errKind    :: ResolutionErrorKind
  , needImport :: Maybe Text
  }

simpleErr :: ResolutionErrorKind -> ResolutionError
simpleErr x = ResolutionError x Nothing

removeSelf :: Resolution -> Text -> [Text]
removeSelf (Resolution (x :| xs)) mod' = map go (x : xs)
  where go x' | x' == mod' = ""
              | otherwise  = x'

instance CompilerError ResolutionError where
  getReport report (ResolutionError kind shouldImport) =
      (solveKind kind) {
        hints = catMaybes
          [ (\x -> "You probably need to import the module '" <> x <> "'") <$> shouldImport
          ]
      }
    where
      solveKind = \case
        (AmbiguousNames range mod' text res) ->
          let err = rangeErr report range [Normal "The name", Marked Main text, Normal "is ambiguous!"]
          in  err { subtitles = map (\t -> (Main, [Normal t])) (removeSelf res mod') }
        (VariableNotFound range text) ->
          rangeErr report range [Normal "Cannot find variable", Marked Main text]
        (ModuleNotFound range text) ->
          rangeErr report range [Normal "Cannot find module", Marked Main text]
        (TypeNotFound range text) ->
          rangeErr report range [Normal "Cannot find type", Marked Main text]
        (ConstructorNotFound range text) ->
          rangeErr report range [Normal "Cannot find data constructor", Marked Main text]
        (DuplicatedPatId range _) ->
          rangeErr report range [Normal "Duplicated pattern name"]

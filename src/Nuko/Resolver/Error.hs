module Nuko.Resolver.Error (
    ResolutionError(..)
) where

import Nuko.Resolver.Support
import Nuko.Error.Data       (CompilerError(..), Colored(..), rangeErr, Marker (Main), subtitles)
import Nuko.Syntax.Range     (Range(..))
import Data.Text             (Text)
import Data.List.NonEmpty    (NonEmpty((:|)))

data ResolutionError
    = AmbiguousNames   Range Text Text Resolution
    | VariableNotFound Range Text
    | ModuleNotFound   Range Text
    | DuplicatedPatId Range Text

removeSelf :: Resolution -> Text -> [Text]
removeSelf (Resolution (x :| xs)) mod' = map go (x : xs)
  where go x' | x' == mod' = ""
              | otherwise  = x'

instance CompilerError ResolutionError where
  getReport report = \case
    (AmbiguousNames range mod' text res) ->
      let err = rangeErr report range [Normal "The resolution of the name", Marked Main text, Normal "is ambiguous!"]
      in  err { subtitles = map (\t -> (Main, [Normal t])) (removeSelf res mod') }
    (VariableNotFound range text) ->
      rangeErr report range [Normal "Cannot find variable", Marked Main text]
    (ModuleNotFound range text) ->
      rangeErr report range [Normal "Cannot find module", Marked Main text]
    (DuplicatedPatId range _) ->
      rangeErr report range [Normal "Duplicated pattern name"]

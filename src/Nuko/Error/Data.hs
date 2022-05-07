module Nuko.Error.Data (
  CompilerError(..),
  Severity(..),
  Report(..),
  Marker(..),
  Colored(..),
  Annotation(..),
  rangeErr,
  onePointErr
) where

import Nuko.Syntax.Range (Point, Range (start), oneColRange)
import Data.Text         (Text)

data Severity = Warn | Error

data Marker
  = Main
  | Sec
  | Third

data Colored
  = Marked Marker Text
  | Normal Text

data Annotation = Ann { range :: Range, marker :: Marker, hint :: (Maybe Text) }

data Report = Report
  { severity     :: Severity
  , title        :: [Colored]
  , subtitles    :: [(Marker, [Colored])]
  , position     :: Point
  , markers      :: [Annotation]

  -- The entire source code
  , sourceCode   :: Text
  , fileName     :: Text

  , hints        :: [Text]
  , debugDetails :: [Text] -- Details that will not be
  }

onePointErr :: Report -> Point -> [Colored] -> Report
onePointErr report point title =
  report
    { severity  = Error
    , title     = title
    , position  = point
    , markers   = [Ann (oneColRange point) Main (Just "Here!")]
    }

rangeErr :: Report -> Range -> [Colored] -> Report
rangeErr report range title =
  report
    { severity  = Error
    , title     = title
    , position  = range.start
    , markers   = [Ann range Main (Just "Here!")]
    }

class CompilerError err where
  getReport   :: Report -> err -> Report

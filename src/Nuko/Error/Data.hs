module Nuko.Error.Data (
  CompilerError(..),
  Severity(..),
  Report(..),
  Marker(..),
  Colored(..),
  Annotation(..)
) where

import Nuko.Syntax.Range (Point, Range)
import Data.Text         (Text)

data Severity = Warn | Error

data Marker = Main | Sec | Third deriving Show

data Colored = Marked Marker Text | Normal Text | Break

data Annotation = Ann { range :: Range, marker :: Marker, hint :: (Maybe Text) } deriving Show

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

class CompilerError err where
  getReport   :: err -> Report

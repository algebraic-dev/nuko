module Nuko.Error.Data (
    CompilerError(..),
    Severity(..),
    Report(..),
    Marker(..),
    Colored(..)
) where

import Nuko.Syntax.Range (Range)
import Data.Text         (Text)

data Severity = Warn | Error

data Marker a = Main a | Sec a | Third a

data Colored a = Marked (Marker Text) | Normal Text

data Report = Report
    { severity    :: Severity
    , title       :: [Colored Text]
    , position    :: Range
    , sourceCodes :: [(Range, Int)]
    , hints       :: [Text]
    }

class CompilerError err where
    getReport   :: err -> Report

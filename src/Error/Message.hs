module Error.Message
  ( Color (..),
    Style (..),
    ErrComponent (..),
    ErrMessage (..),
    ErrReport (..),
    ErrorReport (..),
    normal,
    onlyCol,
    columnError,
    boundsError,
    orZero,
    coloredCode,
    code,
  )
where

import Data.Text (Text)
import Syntax.Range

-- Data Components

data Color
  = Red
  | Blue
  | Green

data Style
  = Normal Text
  | Colored Color Text

data ErrComponent
  = Code Color Range (Maybe Text)
  | Desc Text

data ErrMessage = ErrMessage
  { errRange :: Maybe Pos,
    errTitle :: [Style],
    errComponents :: [ErrComponent]
  }

data ErrReport a = ErrorReport a =>
  ErrReport
  { reportFile :: Text,
    reportContent :: Text,
    reportKind :: a
  }

class ErrorReport a where
  toErrMessage :: a -> ErrMessage

normal :: Text -> [Style]
normal t = [Normal t]

onlyCol :: Pos -> Range
onlyCol pos@(Pos line' col) = Range pos (Pos line' (col + 1))

columnError :: Pos -> [Style] -> ErrMessage
columnError pos text = ErrMessage (Just pos) text [Code Red (onlyCol pos) (Just "Here!!")]

boundsError :: Range -> [Style] -> ErrMessage
boundsError pos text = ErrMessage (Just $ start pos) text [Code Red pos (Just "Here!")]

orZero :: Maybe Range -> Range
orZero (Just b) = b
orZero Nothing = Range (Pos 0 0) (Pos 0 0)

coloredCode :: Color -> Range -> ErrComponent
coloredCode color b = Code color b Nothing

code :: Color -> Text -> Range -> ErrComponent
code color t b = Code color b (Just t)

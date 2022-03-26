module Error.Message where

import Data.Text (Text)
import Syntax.Bounds

-- Data Components

data Color
  = Red
  | Blue
  | Green

data Style
  = Normal Text
  | Colored Color Text

data ErrComponent
  = Code Color Bounds (Maybe Text)
  | Desc Text

data ErrMessage = ErrMessage
  { errBounds :: Maybe Pos,
    errTitle :: [Style],
    errComponents :: [ErrComponent]
  }

data ErrReport a = ErrorReport a => ErrReport
  { reportFile :: Text,
    reportContent :: Text,
    reportKind :: a
  }

class ErrorReport a where
  toErrMessage :: a -> ErrMessage

normal :: Text -> [Style]
normal t = [Normal t]

onlyCol :: Pos -> Bounds
onlyCol pos@(Pos line' col) = Bounds pos (Pos line' (col + 1))

columnError :: Pos -> [Style] -> ErrMessage
columnError pos text = ErrMessage (Just pos) text [Code Red (onlyCol pos) (Just "Here!!")]

boundsError :: Bounds -> [Style] -> ErrMessage
boundsError pos text = ErrMessage (Just $ start pos) text [Code Red pos (Just "Here!")]

orZero :: Maybe Bounds -> Bounds
orZero (Just b) = b
orZero Nothing = Bounds (Pos 0 0) (Pos 0 0)

coloredCode :: Color -> Bounds -> ErrComponent
coloredCode color b = Code color b Nothing

code :: Color -> Text -> Bounds -> ErrComponent
code color t b = Code color b (Just t)

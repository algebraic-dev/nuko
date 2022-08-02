module Nuko.Report.Text (
  Color(..),
  Mode(..),
  Piece(..),
  colorlessFromFormat,
  DetailedDiagnosticInfo(..),
  PrettyDiagnostic(..),
  Severity(..),
  Annotation(..),
  mkBasicDiagnostic,
) where

import Relude

import Nuko.Report.Range (Range)

data Severity
  = Warning
  | Error
  | Information
  | Hint

data Color
  = Fst
  | Snd
  | Thr
  | For

newtype Mode
  = Words [Piece]

data Piece
  = Raw Text
  | Marked Color Text
  | Quoted Piece

data Annotation
  = Ann Color Mode Range
  | NoAnn Color Range

data DetailedDiagnosticInfo = DetailedDiagnosticInfo
  { title     :: Mode
  , subtitles :: [(Color, Mode)]
  , hints     :: [Mode]
  , positions :: [Annotation]
  }

mkBasicDiagnostic :: [Piece] -> [Annotation] -> DetailedDiagnosticInfo
mkBasicDiagnostic title = DetailedDiagnosticInfo (Words title) [] []

class PrettyDiagnostic a where
  prettyDiagnostic :: a -> DetailedDiagnosticInfo

getPieceText :: Piece -> Text
getPieceText (Raw t)      = t
getPieceText (Marked _ t) = t
getPieceText (Quoted t)   = "'" <> getPieceText t <> "'"

colorlessFromFormat :: Mode -> Text
colorlessFromFormat = \case
  (Words pieces) -> unwords (fmap getPieceText pieces)

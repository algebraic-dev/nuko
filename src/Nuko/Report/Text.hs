module Nuko.Report.Text (
  Color(..),
  Mode(..),
  Piece(..),
  colorlessFromFormat
) where

import Relude.String (Text, unwords)
import Relude.Functor (fmap)
import Relude.Monoid ((<>))

data Color = Fst | Snd | Thr

newtype Mode = Words [Piece]
data Piece = Raw Text | Marked Color Text | Quoted Piece

getPieceText :: Piece -> Text
getPieceText (Raw t) = t
getPieceText (Marked _ t) = t
getPieceText (Quoted t) = "'" <> getPieceText t <> "'"

colorlessFromFormat :: Mode -> Text
colorlessFromFormat = \case
  (Words pieces) -> unwords (fmap getPieceText pieces)
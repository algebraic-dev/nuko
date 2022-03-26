-- | This module pretty prints the data structures
-- created in the Error.Message module. It does it by 
-- working with a lot of texts and generating a text in the
-- end.
module Error.PrettyPrint where

import Data.Text (Text)
import Syntax.Bounds (Bounds, Pos)
import System.Console.Pretty (Pretty (bgColor, color, style), Style (Faint))

import qualified Data.Text as T
import qualified Text.Printf as P
import qualified Error.Message as M
import qualified Syntax.Bounds as B
import qualified System.Console.Pretty as SCP

toColor :: M.Color -> SCP.Color
toColor = \case
  M.Red -> SCP.Red
  M.Blue -> SCP.Blue
  M.Green -> SCP.Green

space :: Int -> Text
space n = T.replicate n " "

subString :: Int -> Int -> Text -> Text
subString st_col end_col = T.take (end_col - st_col) . T.drop st_col

splitText :: Int -> Int -> Text -> (Text, Text, Text)
splitText start end text =
  let startText = subString 0 start text
      middleText = subString start end text
      endText = subString end (T.length text) text
   in (startText, middleText, endText)

colorizeLine :: SCP.Color -> Int -> Int -> Text -> Text
colorizeLine clr start end text =
  let (startText, middleText, endText) = splitText (start - 1) (end - 1) text
   in T.concat [startText, color clr middleText, endText]

ppLine :: Int -> Text -> Text
ppLine line = T.pack . P.printf "%8d | %s\n" (line + 1)

ppNote :: SCP.Color -> Int -> Int -> Maybe Text -> Text
ppNote clr col len (Just text) = T.concat [space (col + 10), color clr (T.append "└" (T.replicate (len - 1) "─")), " ", color clr text, "\n"]
ppNote _ _ _ Nothing = ""

-- I'll probably have to rewrite this lol. it's ugly as fuck
ppLines :: Maybe Text -> SCP.Color -> Text -> Bounds -> Text
ppLines note clr source (B.Bounds start end)
  | B.line start == B.line end =
    let line = T.lines source !! B.line start
     in T.concat
          [ ppLine' (B.line start) (B.column start) (B.column end) line,
            ppNote clr (B.column start) (B.column end - B.column start) note
          ]
  | otherwise =
    let lines' = T.lines source
        line = lines' !! B.line start
        line' = lines' !! B.line end
        fstLine = ppLine' (B.line start) (B.column start) (T.length line) line
        sndLine = ppLine' (B.line end) 0 (B.column end) line'
     in T.unlines [fstLine, ppNote clr (B.column start) (T.length line - B.column start) note, sndLine]
  where
    ppLine' line st en = ppLine line . colorizeLine clr st en

ppFile :: Text -> Maybe Pos -> Text
ppFile text (Just (B.Pos line col)) = T.pack $ P.printf "%s ──> %s:%d:%d\n\n" (space 7) text line col
ppFile text Nothing = T.pack $ P.printf "%s ──> %s\n\n" (space 4) text

ppTitle :: Text -> Text
ppTitle t = T.concat [space 4, bgColor SCP.Red " ERROR ", " ", t, "\n"]

ppComponent :: Text -> M.ErrComponent -> Text
ppComponent source (M.Code color' bounds note) = T.append (ppLines note (toColor color') source bounds) "\n"
ppComponent _ (M.Desc t) = T.concat [space 4, t, "\n\n"]

ppErrorMessage :: Text -> Text -> M.ErrMessage -> Text
ppErrorMessage source subLine (M.ErrMessage _ title components) =
  T.concat $
    [ "\n",
      ppTitle (T.concat $ map remStyle title),
      color SCP.Cyan $ style Faint subLine
    ]
      ++ map (ppComponent source) components
  where
    remStyle = \case
      M.Normal t -> style SCP.Bold t
      M.Colored color' t -> style SCP.Bold $ color (toColor color') t

ppErrorReport :: M.ErrorReport a => M.ErrReport a -> Text
ppErrorReport (M.ErrReport file content kind) =
  let beKind = M.toErrMessage kind
   in ppErrorMessage content (ppFile file (M.errBounds beKind)) beKind
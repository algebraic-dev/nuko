module Nuko.Report.Render (
  renderDiagnostic
) where

import Relude                 hiding (intercalate, replicate, unlines, unwords)

import Data.Text              (replicate)
import Nuko.Report.Message    (Diagnostic (..))
import Nuko.Report.Range      (Pos (Pos, column, line), Range (..))
import Pretty.Format          (Format (..))
import Relude.Unsafe          ((!!))

import Data.Text.Lazy.Builder (Builder, fromText, toLazyText)

import Data.List              qualified as List
import Data.Text              qualified as Text
import Nuko.Report.Text       qualified as Report
import System.Console.Pretty  qualified as Pretty

indent :: Int
indent = 5

bold :: Text -> Text
bold = Pretty.style Pretty.Bold

intercalate :: Builder -> [Builder] -> Builder
intercalate sep list = case list of
  []     -> mempty
  [x]    -> x
  (x:xs) -> x <> sep <> intercalate sep xs

unwords :: [Builder] -> Builder
unwords = intercalate " "

unlines :: [Builder] -> Builder
unlines = intercalate "\n"

pad :: Int -> Builder
pad n = fromText $ Text.replicate n " "

faint :: Text -> Text
faint = Pretty.style Pretty.Faint

blue :: Text -> Text
blue = Pretty.color Pretty.Blue

builderFaint :: Text -> Builder
builderFaint = fromText . faint

colorize :: Pretty.Color -> Text -> Builder
colorize color = fromText . Pretty.color color

bgColorize :: Pretty.Color -> Text -> Builder
bgColorize color = fromText . Pretty.bgColor color

renderBadge :: Report.Severity -> Builder
renderBadge = \case
    Report.Error       -> bgColorize Pretty.Red " ERROR "
    Report.Warning     -> bgColorize Pretty.Yellow " WARN "
    Report.Information -> bgColorize Pretty.Blue " INFO "
    Report.Hint        -> bgColorize Pretty.Green " Hint "

getColorFromMark :: Report.Color -> Pretty.Color
getColorFromMark = \case
  Report.Fst -> Pretty.Red
  Report.Snd -> Pretty.Blue
  Report.Thr -> Pretty.Green
  Report.For -> Pretty.Yellow

getPieceText :: (Text -> Text) -> Report.Piece -> Builder
getPieceText trans = \case
  Report.Raw t      -> fromText (trans t)
  Report.Marked m t -> colorize (getColorFromMark m) (trans t)
  Report.Quoted t   -> "'" <> getPieceText trans t <> "'"

renderMode :: (Text -> Text) -> Report.Mode -> Builder
renderMode trans = \case
  Report.Words words' -> unwords (getPieceText trans <$> words')

renderTitle :: Diagnostic -> Report.DetailedDiagnosticInfo -> Builder
renderTitle diagnostic detailed =
  renderBadge diagnostic.severity <> " " <> renderMode bold detailed.title

renderLocation :: Diagnostic -> Builder
renderLocation diagnostic =
  let pos = diagnostic.position in
  fromText (blue $ faint (diagnostic.filename <> ":" <> format (pos.start.line + 1) <> ":" <> (format $ pos.start.column + 1)))

renderSubtitle :: (Report.Color, Report.Mode) -> Builder
renderSubtitle (color, text) =
  pad indent <> colorize (getColorFromMark color) "• " <> renderMode bold text

renderSubtitles :: [(Report.Color, Report.Mode)] -> Builder
renderSubtitles [] = mempty
renderSubtitles ts = unlines (renderSubtitle <$> ts) <> "\n\n"

modifyLine :: [Text] -> Range -> (Text -> Text) -> Builder
modifyLine source range fn =
    let (start', rest) = Text.splitAt fixedRange.start.column line'
        (middle, end') = Text.splitAt (fixedRange.end.column - fixedRange.start.column) rest
    in fromText $ start' <> fn middle <> end'
  where
    line' = source !! range.start.line
    toEndRange (Range start' end') size = Range start' (Pos end'.line size)
    sameLine   = range.start.line == range.end.line
    fixedRange = if sameLine then range else toEndRange range (Text.length line')

emptyLine :: Builder
emptyLine = pad indent <> fromText (faint "┊ \n")

-- TODO: Probably join these two functions?
renderLine :: [Text] -> Int -> Builder
renderLine source lineNum | lineNum < 0 || lineNum >= List.length source = emptyLine
renderLine source lineNum = do
  let lineText = format (lineNum + 1)
  let padding  = pad (indent - Text.length lineText - 1)
  let line'    = source !! lineNum
  let header   = fromText $ faint $ lineText <> " | "
  padding <> header <> fromText (faint line') <> "\n"

renderPosition :: [Text] -> Report.Annotation -> Builder
renderPosition source ann = do
    let lineText = format (range.start.line + 1)
    let padding  = pad (indent - Text.length lineText - 1)
    let tip      = maybe "" (("\n" <> pad indent <> fromText (faint "| ") <> pad range.start.column <> fromText (boldColor $ replicate (List.maximum [colSize, 1]) "^" <> " ")) <>) text
    let resLine  = padding <> fromText lineText <> " | " <> modifyLine source range boldColor <> tip
    mconcat
      [ renderLine source (range.start.line - 1)
      , resLine <> "\n"
      , renderLine source (range.start.line + 1)
      , memptyIfFalse (range.start.line + 1 < List.length source) emptyLine
      ]
  where
    boldColor :: Text -> Text
    boldColor t = Pretty.style Pretty.Bold $ Pretty.color color t
    line'     = source !! range.start.line
    colSize  = if range.start.line == range.end.line then range.end.column - range.start.column else Text.length line' - range.start.column
    color = getColorFromMark $ case ann of {Report.Ann color' _ _ -> color'; Report.NoAnn color' _ -> color' }
    range = case ann of {Report.Ann _ _ range' -> range'; Report.NoAnn _ range' -> range' }
    text  = renderMode boldColor <$> case ann of {Report.Ann _ text' _ -> Just text'; Report.NoAnn {} -> Nothing }

renderDiagnostic :: [Text] -> Diagnostic -> Text
renderDiagnostic source diagnostic =
    toStrict $ toLazyText go
  where
    detailed = Report.prettyDiagnostic diagnostic.kind
    go =
      mconcat
        [ "\n"
        , pad 1 <> renderTitle diagnostic detailed <> "\n"
        , "\n"
        , renderSubtitles detailed.subtitles
        , pad indent <> builderFaint "┌" <> (fromText (blue $ faint " at ") <> renderLocation diagnostic <> "\n")
        , pad indent <> builderFaint "|" <> "\n"
        , mconcat $ renderPosition source <$> detailed.positions
        ]

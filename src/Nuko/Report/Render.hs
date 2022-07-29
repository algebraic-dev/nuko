module Nuko.Report.Render (
  renderDiagnostic
) where

import Nuko.Report.Text
import Nuko.Report.Message    (Diagnostic(..))
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText)

import Relude.String     (Text)
import Relude.Function   ((.), ($))
import Relude.Monoid     ((<>), Monoid (mempty, mconcat), memptyIfFalse)
import Relude.Functor    ((<$>))
import Relude            (Int, Eq ((==)), Num ((-), (+)), Maybe (..), maybe, LazyStrict (toStrict), Ord (..))
import Data.Text         (replicate, length, splitAt)
import Nuko.Report.Range (Range(..), Pos(Pos, line, column))
import Relude.Unsafe     ((!!))
import Pretty.Format     (Format(..))
import Data.Bool         ((||))

import qualified System.Console.Pretty as Pretty
import qualified Data.List as List

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
pad n = fromText $ replicate n " "

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

renderBadge :: Severity -> Builder
renderBadge = \case
    Error       -> bgColorize Pretty.Red " ERROR "
    Warning     -> bgColorize Pretty.Yellow " WARN "
    Information -> bgColorize Pretty.Blue " INFO "
    Hint        -> bgColorize Pretty.Green " Hint "

getColorFromMark :: Color -> Pretty.Color
getColorFromMark = \case
  Fst -> Pretty.Red
  Snd -> Pretty.Blue
  Thr -> Pretty.Green

getPieceText :: (Text -> Text) -> Piece -> Builder
getPieceText trans = \case
  Raw t      -> fromText (trans t)
  Marked m t -> colorize (getColorFromMark m) (trans t)
  Quoted t   -> "'" <> getPieceText trans t <> "'"

renderMode :: (Text -> Text) -> Mode -> Builder
renderMode trans = \case
  Words words -> unwords (getPieceText trans <$> words)

renderTitle :: Diagnostic -> DetailedDiagnosticInfo -> Builder
renderTitle diagnostic detailed =
  renderBadge diagnostic.severity <> " " <> renderMode bold detailed.title

renderLocation :: Diagnostic -> Builder
renderLocation diagnostic =
  let pos = diagnostic.position in
  fromText (blue $ faint $ (diagnostic.filename <> ":" <> (format $ pos.start.line + 1) <> ":" <> (format $ pos.start.column + 1)))

renderSubtitle :: (Color, Mode) -> Builder
renderSubtitle (color, text) =
  pad indent <> colorize (getColorFromMark color) "• " <> renderMode bold text

renderSubtitles :: [(Color, Mode)] -> Builder
renderSubtitles [] = mempty
renderSubtitles ts = unlines (renderSubtitle <$> ts) <> "\n\n"

modifyLine :: [Text] -> Range -> (Text -> Text) -> Builder
modifyLine source range fn =
    let (start, rest) = splitAt fixedRange.start.column line
        (middle, end) = splitAt (fixedRange.end.column - fixedRange.start.column) rest
    in fromText $ start <> fn middle <> end
  where
    line = source !! range.start.line
    toEndRange (Range start end) size = Range start (Pos end.line size)
    sameLine   = range.start.line == range.end.line
    fixedRange = if sameLine then range else toEndRange range (length line)

emptyLine :: Builder
emptyLine = pad (indent) <> (fromText $ faint "┊ \n")

-- TODO: Probably join these two functions?
renderLine :: [Text] -> Int -> Builder
renderLine source lineNum | lineNum < 0 || lineNum >= List.length source = emptyLine
renderLine source lineNum = do
  let lineText = format (lineNum + 1)
  let padding  = pad (indent - length lineText - 1)
  let line     = source !! lineNum
  let header   = fromText $ faint $ lineText <> " | "
  padding <> header <> (fromText $ faint $ line) <> "\n"

renderPosition :: [Text] -> Annotation -> Builder
renderPosition source ann = do
    let lineText = format (range.start.line + 1)
    let padding  = pad (indent - length lineText - 1)
    let tip      = maybe "" (("\n" <> pad indent <> fromText (faint "| ") <> pad range.start.column <> fromText (boldColor "└──")) <>) text
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
    color = getColorFromMark $ case ann of {Ann color' _ _ -> color'; NoAnn color' _ -> color' }
    range = case ann of {Ann _ _ range' -> range'; NoAnn _ range' -> range' }
    text  = renderMode boldColor <$> case ann of {Ann _ text' _ -> Just text'; NoAnn {} -> Nothing }

renderDiagnostic :: [Text] -> Diagnostic -> Text
renderDiagnostic source diagnostic =
    toStrict $ toLazyText go
  where
    detailed = prettyDiagnostic diagnostic.kind
    go =
      mconcat
        [ "\n"
        , pad 1 <> renderTitle diagnostic detailed <> "\n"
        , "\n"
        , renderSubtitles detailed.subtitles
        , pad indent <> builderFaint "┌" <> (fromText (blue $ faint $ " at ") <> renderLocation diagnostic <> "\n")
        , pad indent <> builderFaint "|" <> "\n"
        , unlines $ renderPosition source <$> detailed.positions
        ]
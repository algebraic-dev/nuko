module Nuko.Error.Render (prettyPrint) where

import Nuko.Error.Data
import Nuko.Syntax.Range        (Range(..), Point(..))
import Data.Text                (Text)
import Data.Maybe               (isJust)
import Data.List.NonEmpty       (NonEmpty ((:|)))

import qualified Data.Text             as Text
import qualified System.Console.Pretty as Pretty
import qualified Data.List             as List

(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.) . (.)

dull :: Text -> Text
dull = Pretty.style Pretty.Faint

bold :: Text -> Text
bold = Pretty.style Pretty.Bold

cyan :: Text -> Text
cyan = Pretty.color Pretty.Cyan

pad :: Int -> Text
pad = flip Text.replicate " "

leftPad :: Int -> Text -> Text
leftPad padding text = pad (padding - Text.length text) <> text

lined :: Int -> [Text] -> Text
lined padding = (pad padding <>) . (<> "\n") . Text.intercalate " "

renderMarked :: Marker -> Text -> Text
renderMarked = bold .: \case
  Main -> Pretty.color Pretty.Red
  Sec -> Pretty.color Pretty.Blue
  Third -> Pretty.color Pretty.Green

renderColored :: Colored -> Text
renderColored = \case
  Marked color text -> renderMarked color text
  Normal text       -> bold text

severityTag :: Severity -> Text
severityTag = \case
  Warn  -> " WARN "
  Error -> " ERROR "

severityColor :: Severity -> Pretty.Color
severityColor = \case
  Warn  -> Pretty.Yellow
  Error -> Pretty.Red

renderSeverity :: Severity -> Text
renderSeverity tag = Pretty.bgColor (severityColor tag) (severityTag tag)

renderTitle :: [Colored] -> Text
renderTitle title = Text.unwords (map renderColored title)

renderHeader :: Int -> Severity -> [Colored] -> Text
renderHeader padding severity title =
  let renderedTag   = renderSeverity severity
      renderedTitle = renderTitle title
  in mconcat [ lined padding [renderedTag, renderedTitle] , "\n"]

renderLocation :: Int -> Text -> Point -> Text
renderLocation padding fileName position =
  mconcat
    [ lined (padding + 2) [(dull " ┌ " <> dull (cyan ("at " <> fileName <> ":" <> (Text.pack $ show position))))]
    , lined (padding + 2) [dull " │"]
    ]

modifySection :: (Text -> Text) -> Range -> Text -> Text
modifySection fn (Range start end) text =
  let (tStart, stretch) = Text.splitAt start.column text
      (tMiddle, tEnd)   = Text.splitAt (end.column - start.column) stretch
  in tStart <> fn tMiddle <> tEnd

splitAnnotation :: [Text] -> Annotation -> [Annotation]
splitAnnotation code (Ann range marker text)
    | sL == eL  = [ Ann range marker text ]
    | otherwise = [ Ann (Range range.start (Point sL (Text.length (code !! sL)))) marker text
                  , Ann (Range (Point eL 0) range.end) marker Nothing ]
  where
    sL = range.start.line
    eL = range.end.line

groupLine :: [Text] -> [Annotation] -> (Int, NonEmpty Text)
groupLine codeLines group =
      let line     = getGroupLine group
          mainLine = foldl (flip sectionMod) (codeLines !! line) (reverseOrder group)
      in (line, mainLine :| renderMarks)
  where
    renderMarks = foldr (\ann l -> makeLine 0 ann : l) []
      $ List.filter (not . null)
      $ List.reverse
      $ List.inits
      $ List.sortBy compareColumn
      $ List.filter (isJust . hint) group

    makeLine _ [] = ""

    makeLine offset [Ann range marker (Just hint)] =
      pad (range.start.column - offset)
      <> renderMarked marker ("└── " <> hint)

    makeLine offset (Ann range marker _ : xs) =
      pad (range.start.column - offset)
      <> renderMarked marker ("│" <> makeLine (range.start.column + 1) xs)

    sectionMod (Ann r m _) = modifySection (renderMarked m) r
    getGroupLine ls = line $ start $ range $ head ls
    reverseOrder           = List.sortBy (flip compareColumn)
    compareColumn (Ann r1 _ _) (Ann r2 _ _) = compare r1.start.column r2.start.column

groupLines :: [Annotation] -> Text -> [(Int, NonEmpty Text)]
groupLines markers code =
    let codeLines = Text.lines code
        sortedMarkers = List.groupBy sameLine $ List.sortBy compareLine $ concatMap (splitAnnotation codeLines) markers
    in map (groupLine codeLines) sortedMarkers
  where
    compareLine   (Ann r1 _ _) (Ann r2 _ _) = compare r1.start.line r2.start.line
    sameLine (Ann r1 _ _) (Ann r2 _ _) = r1.start.line == r2.start.line

renderLines :: Int -> [(Int, NonEmpty Text)] -> Text
renderLines _ [] = ""
renderLines offset ((l, text :| texts) : xs) =
    let line' = dull (leftPad (offset + 2) (Text.pack $ show (l + 1)) <> " │ ") <> text
        marks = map (dullStart <>) texts
    in Text.unlines (line' : marks) <> (if null marks then "" else dullStart <> "\n") <> renderLines offset xs
  where
    dullStart = dull $ leftPad (offset + 5) "┆ "

renderSubtitles :: Int -> [(Marker, [Colored])] -> Text
renderSubtitles padding sub =
    Text.unlines $ map renderSub sub
  where
    renderSub (marker, title) = mconcat [pad (padding + 2), renderMarked marker " • ", renderTitle title]

renderHint :: Int -> [Text] -> Text
renderHint _ []       = ""
renderHint padding (x : xs) =
  cyan (bold (leftPad (padding + 6) "Hint: ") <> cyan x) <> "\n"
  <> Text.unlines (map (cyan . (pad (padding + 6) <>)) xs)

prettyPrint :: Int -> Report -> String
prettyPrint padding report = Text.unpack $
    mconcat
      [ "\n"
      , renderHeader padding report.severity report.title
      , renderSubtitles padding report.subtitles
      , nullJump report.subtitles
      , renderLocation padding report.fileName report.position
      , renderLines padding $ groupLines report.markers report.sourceCode
      , nullJump report.hints
      , renderHint padding report.hints
      ]
  where
    nullJump :: [a] -> Text
    nullJump x = if null x then "" else "\n"
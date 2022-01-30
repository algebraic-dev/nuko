module Error.PrettyPrint where

import Data.Text (Text)
import Error.Message (ErrReport)
import Syntax.Bounds (Bounds, Pos)

import System.Console.Pretty ( Color(Cyan, Red), Pretty(color, bgColor, style), Style(Faint, Bold) )
import Text.Printf ( printf )

import qualified Syntax.Bounds as B
import qualified Error.Message as M
import qualified Data.Text as T

trimLines :: [(Int, a)] -> [(Int, a)]
trimLines []    = []
trimLines [x]   = [x]
trimLines other = [head other, last other]

getLines :: Bounds -> Text -> [(Int, Text)]
getLines bounds text =
        let lines' = T.lines text
            start  = B.line (B.start bounds)
            len    = max (start - B.line (B.end bounds)) 1
        in trimLines $
            zip (map (+ (start + 1)) [0..len])
                (take len (drop start lines'))

badge :: Text -> Color -> Text
badge kind color' = bgColor color' (T.concat [" ", kind ," "])

showLine :: Text -> Text
showLine title = T.pack $ printf " %s %s" (badge "ERROR" Red) title

showCode :: Text -> Maybe Pos -> Text
showCode file (Just pos) = T.pack $ printf "%s:%d:%d" file (B.line pos + 1) (B.column pos)
showCode _ Nothing = ""

showCodeLine :: (Int, Text) -> Text
showCodeLine (num, text) = T.pack $ printf "%4d  | %s" num text

space :: Int -> Text
space len = T.pack (replicate len ' ')

showTip :: Pos -> Text -> Text
showTip pos text = T.pack $ style Bold
                          $ color Red
                          $ printf "%*s◮  %s" (B.column pos + 7) (" " :: String) text

ppShow :: ErrReport -> Text
ppShow (M.ErrReport file content kind) =
    let message = M.messageFromErr kind in
    "\n" <> T.intercalate "\n\n"
                [ showLine (M.errTitle message)
                , T.concat ["   ", style Faint . color Cyan $ showCode file (M.errBounds message)]
                , T.unlines (map ppShowMessage (M.errComponents message)) ]
    where
        ppShowMessage (M.Desc text) = T.concat [ "   ", text, "\n" ]
        ppShowMessage (M.Code bounds anot) =
                T.unlines $ map showCodeLine (getLines bounds content) ++ tip anot
            where
                tip (Just tip') = [showTip (B.start bounds) tip']
                tip Nothing = []   
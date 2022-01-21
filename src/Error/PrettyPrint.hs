module Error.PrettyPrint where 

import Data.Text (Text)
import Error.Message (ErrReport)
import Syntax.Bounds (Bounds, Pos)

import qualified Syntax.Bounds as B
import qualified Error.Message as M 
import qualified Data.Text as T
import qualified System.Console.Pretty as P

trimLines :: [(Int, Text)] -> [(Int, Text)]
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

showLine :: Text -> Text -> P.Color -> Text 
showLine title type' color = T.concat [ " ", P.bgColor color " " ++ type'  ++ " ", " ", title]

showCode :: Text -> Maybe Pos -> Text 
showCode        _ Nothing = ""
showCode fileName (Just (B.Pos line column)) = 
    T.concat [ fileName, ":", T.pack $ show line, ":", T.pack $ show column ]

showCodeLine :: (Int, Text) -> Text 
showCodeLine (num, text) = 
    T.concat [ T.pack $ replicate (4 - length (show num)) ' '
             , T.pack $ show num
             , " | "
             , text]

ppShow :: ErrReport -> Text 
ppShow (M.ErrReport file content kind) = 
        let message = M.messageFromErr kind in
        T.intercalate "\n\n"  
            [ ""
            , showLine (M.errTitle message) "ERROR" P.Red
            , T.concat ["   ", colorize ("on " ++ showCode file (M.errBounds message))]
            , T.unlines (ppShowMessage (M.errComponents message)) ]
    where
        colorize = P.style P.Faint . P.color P.Cyan

        ppShowMessage (M.Code bounds : M.Tip text : tl) = 
            let col   = B.column . B.start $ bounds
                anot  = T.concat [T.pack (replicate (col + 6) ' '),"◮  ", text ] 
            in T.unlines (map showCodeLine (getLines bounds content)  ++ [anot]) 
               : ppShowMessage tl
        
        ppShowMessage (M.Code bounds : tl) = 
            T.unlines (map showCodeLine (getLines bounds content)) : ppShowMessage tl
        
        ppShowMessage (M.Tip text : tl) = 
            T.concat [ "   ", text, "\n" ] : ppShowMessage tl
        
        ppShowMessage (M.Desc text : tl) = 
            T.concat [ "   ", text, "\n" ] : ppShowMessage tl
            
        ppShowMessage [] = []
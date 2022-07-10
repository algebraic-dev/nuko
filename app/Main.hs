module Main (main) where

import Relude                    (Monad((>>)), IO, forM_, print, putStrLn, putText, Text )
import Data.These                (These(..))
import Nuko.Syntax.Lexer.Support (runLexer)
import Nuko.Syntax.Parser        (parseProgram)
import Pretty.Tree               (PrettyTree(..))

import qualified Data.ByteString as IO

main :: IO ()
main = do
  r <- IO.readFile "examples/normal.nk"
  print ("Stargin.." :: Text)
  case runLexer parseProgram r of
    That a    -> putStrLn "V" >> putText (prettyShowTree a)
    This a    -> putStrLn "X" >> forM_ a print
    These a b -> putStrLn "?" >> forM_ a print >> putText (prettyShowTree b)
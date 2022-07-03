module Main (main) where

import Relude
import Nuko.Syntax.Lexer.Support (runLexer)
import Nuko.Syntax.Lexer (scan)
import Nuko.Syntax.Parser (parseProgram)
import qualified Data.ByteString as IO
import Data.These

import Pretty.Tree

main :: IO ()
main = do
  r <- IO.readFile "examples/normal.nk"
  print ("Stargin.." :: Text)
  case runLexer parseProgram r of
    That a    -> putStrLn "V" >> putText (prettyShowTree a)
    This a    -> putStrLn "X" >> forM_ a print
    These a b -> putStrLn "?" >> forM_ a print >> putText (prettyShowTree b)
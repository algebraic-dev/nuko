module Main (main) where

import Relude
import Nuko.Syntax.Lexer.Support (runLexer)
import Nuko.Syntax.Lexer (scan)
import Nuko.Syntax.Parser (parseProgram)
import qualified Data.ByteString as IO
import Data.These

main :: IO ()
main = do
  r <- IO.readFile "examples/normal.nk"
  print "Stargin.."
  case runLexer parseProgram r of
    That a    -> putStrLn "V" >> print a
    This a    -> putStrLn "X" >> forM_ a print
    These a b -> putStrLn "?" >> forM_ a print >> print b
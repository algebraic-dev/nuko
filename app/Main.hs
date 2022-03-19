module Main where

import qualified Data.ByteString as SB
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Pretty.Tree (drawTree)
import System.Environment (getArgs)

import Error.Message (ErrReport (ErrReport))
import Error.PrettyPrint (ppErrorReport)

import Syntax.Lexer.Support (runLexer)
import Syntax.Parser (parseProgram)

main :: IO ()
main = do
  setLocaleEncoding utf8
  [file] <- getArgs
  str <- SB.readFile file
  case runLexer parseProgram str of
    Right res -> putStrLn $ drawTree res
    Left err -> putStrLn (unpack $ ppErrorReport $ ErrReport (pack file) (decodeUtf8 str) err)
module Main (main) where

import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Pretty.Tree (drawTree)
import System.Environment (getArgs)

import Error.PrettyPrint (ppErrorReport)
import Error.Message (ErrReport (ErrReport))

import Syntax.Parser ( parseType ) 
import Syntax.Lexer.Support (runLexer)

import qualified Data.ByteString as SB

main :: IO ()
main = do
  setLocaleEncoding utf8
  [file] <- getArgs
  str <- SB.readFile file
  case runLexer parseType str of
    Right res -> putStrLn (drawTree res)
    Left err -> putStrLn (unpack $ ppErrorReport $ ErrReport (pack file) (decodeUtf8 str) err) 
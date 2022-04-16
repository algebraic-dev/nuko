module Main (main) where

import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Pretty.Tree (drawTree)
import System.Environment (getArgs)

import Error.PrettyPrint (ppErrorReport)
import Error.Message (ErrReport (ErrReport))

import Syntax.Parser ( parseProgram ) 
import Syntax.Lexer.Support (runLexer)

import qualified Data.ByteString as SB
import Syntax.Lexer (scan)
import Control.Monad (replicateM)

main :: IO ()
main = do
  setLocaleEncoding utf8
  [file] <- getArgs
  str <- SB.readFile file
  case runLexer (replicateM 10 scan) str of
    Right res -> putStrLn (show res)
    Left err -> putStrLn (unpack $ ppErrorReport $ ErrReport (pack file) (decodeUtf8 str) err) 
module Main where

import GHC.IO.Encoding ( utf8, setLocaleEncoding )
import Syntax.Lexer.Support ( runLexer )
import Syntax.Parser ( parseExpr )
import System.Environment ( getArgs )

import Data.Text.Encoding (decodeUtf8)
import Data.Text (pack, unpack)

import Error.Message ( ErrReport(ErrReport) )
import Error.PrettyPrint ( ppShow )
import Pretty.Tree ( drawTree )

import qualified Data.ByteString as SB

main :: IO ()
main = do
  setLocaleEncoding utf8
  [file] <- getArgs
  str    <- SB.readFile file
  case runLexer parseExpr str of
      Right res -> do
        putStrLn $ drawTree res
      Left err -> putStrLn (unpack $ ppShow $ ErrReport (pack file) (decodeUtf8 str) err)
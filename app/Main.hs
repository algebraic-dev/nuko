module Main where 

import GHC.IO.Encoding

import Syntax.Lexer.Support
import Syntax.Parser

import System.Environment

import qualified Data.ByteString as SB
import Data.Text.Encoding (decodeUtf8)
import Data.Text (pack, unpack)

import Syntax.Tree
import Error.Message
import Error.PrettyPrint
import Syntax.Expr 
import Syntax.Parser.Ast

import Data.Set (Set, toList)

main :: IO ()
main = do  
  setLocaleEncoding utf8
  [file] <- getArgs

  bs  <- SB.readFile file 
  case (runLexer parseProgram bs) of 
      Right res@(Program _ b _ _) -> do 
        putStrLn $ drawTree res
        putStrLn "\n------------------\n"
        let f = map astFreeVars b
        let name = map (\(Name (_, t)) -> t)
        print (map (name . toList) f)
      Left err -> putStrLn (unpack $ ppShow $ ErrReport (pack file) (decodeUtf8 bs) err)
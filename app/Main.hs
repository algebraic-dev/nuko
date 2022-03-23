module Main where

import qualified Data.ByteString as SB
import Data.Text (pack, unpack, Text)
import Data.Text.Encoding (decodeUtf8)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Pretty.Tree (drawTree)
import System.Environment (getArgs)

import Error.Message (ErrReport (ErrReport))
import Error.PrettyPrint (ppErrorReport)

import Syntax.Lexer.Support (runLexer)
import Syntax.Parser (parseProgram)

import Typer.Kinds 

import qualified Data.Map as Map
import Syntax.Bounds (Bounds (Bounds), Pos (Pos))
import Syntax.Parser.Ast (Normal)
import Syntax.Expr 

ctx :: Ctx
ctx = Ctx 0 0 Map.empty
              (Map.fromList [("Int", Star)])
              Map.empty

bb :: Bounds
bb = Bounds (Pos 0 0) (Pos 0 0)

name :: Text -> Name Normal
name = Name bb

simple :: Text -> Typer Normal 
simple = TSimple NoExt . name

forall :: Text -> Typer Normal -> Typer Normal 
forall t = TForall bb (name t)

(~>) :: Typer Normal  -> Typer Normal  -> Typer Normal 
(~>) = TArrow bb

app :: Typer Normal -> Typer Normal -> Typer Normal  
app = TApp bb

main :: IO ()
main = do
  setLocaleEncoding utf8
  [file] <- getArgs
  str <- SB.readFile file
  case runLexer parseProgram str of
    Right res -> putStrLn $ drawTree res
    Left err -> putStrLn (unpack $ ppErrorReport $ ErrReport (pack file) (decodeUtf8 str) err) 
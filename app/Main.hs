module Main (main) where

import Data.Text (pack, unpack, Text)
import Data.Text.Encoding (decodeUtf8)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Environment (getArgs)
import Error.PrettyPrint (ppErrorReport)
import Error.Message (ErrReport (ErrReport))
import Syntax.Lexer.Support (runLexer)
import Typer.Infer (inferExpr)
import Typer.Env (Env(Env), runEnv, Ty (TyForall, TyNamed, TyFun))
import Typer.Error (TypeError)
import Control.Exception (catch)
import Data.Map (Map)
import Syntax.Range (Loc(Blank))
import Syntax.Parser (parseExpr)

import qualified Data.ByteString as SB
import qualified Data.Map as Map

vars :: Map Text Ty
vars = Map.fromList
  [
    ("k", TyForall Blank "b" (TyFun Blank (TyForall Blank "a" (TyNamed Blank "a")) (TyNamed Blank "b")))
  , ("f1", TyFun Blank ( (TyFun Blank (TyNamed Blank "Int") (TyFun Blank (TyNamed Blank "Int") (TyNamed Blank "Int")))) (TyNamed Blank "Int"))
  , ("f2", TyFun Blank (TyForall Blank "a" (TyFun Blank (TyNamed Blank "a") (TyFun Blank (TyNamed Blank "a") (TyNamed Blank "a")))) (TyNamed Blank "Int"))

  ]

main :: IO ()
main = do
  setLocaleEncoding utf8
  [file] <- getArgs
  str <- SB.readFile file
  case runLexer parseExpr str  of
    Right res -> do
      catch (runEnv (inferExpr res) (Env 0 0 [] vars mempty) >>= print)
            ((\err -> putStrLn (unpack $ ppErrorReport $ ErrReport (pack file) (decodeUtf8 str) err)) :: TypeError -> IO ())
    Left err -> putStrLn (unpack $ ppErrorReport $ ErrReport (pack file) (decodeUtf8 str) err)
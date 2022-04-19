module Main (main) where

import Data.Text            (pack, unpack, Text)
import Data.Text.Encoding   (decodeUtf8)
import GHC.IO.Encoding      (setLocaleEncoding, utf8)
import System.Environment   (getArgs)
import Error.PrettyPrint    (ppErrorReport)
import Error.Message        (ErrReport (ErrReport))
import Syntax.Lexer.Support (runLexer)
import Typer.Infer          (inferExpr)
import Typer.Env            (Env(Env), runEnv, Ty (TyForall, TyNamed, TyFun))
import Typer.Error          (TypeError)
import Control.Exception    (catch)
import Data.Map             (Map)
import Syntax.Parser        (parseExpr)

import qualified Data.ByteString as SB
import qualified Data.Map        as Map
import qualified Data.Sequence   as Seq
import qualified Data.Set        as Set

infixr 3 ~>

v :: Text -> Ty
v = TyNamed Nothing

(~>) :: Ty -> Ty -> Ty
(~>) = TyFun Nothing

forall' :: Text -> Ty -> Ty
forall' = TyForall Nothing

vars :: Map Text Ty
vars = Map.fromList
  [
    ("k",  forall' "b" (forall' "a" (v "a")) ~> (v "b"))
  , ("f1", (v "Int" ~> v "Int" ~> v "Int") ~> v "Int")
  , ("f2", (forall' "a" ((v "a") ~> v "a" ~> v "a")) ~> v "Int")
  , ("g", forall' "b" (forall' "a" (v "Int")) ~> (v "Int"))
  , ("g1", forall' "b" (forall' "a" (v "Bool")))
  ]

main :: IO ()
main = do
  setLocaleEncoding utf8
  [file] <- getArgs
  str <- SB.readFile file
  case runLexer parseExpr str  of
    Right res -> do
      catch (runEnv (inferExpr res) (Env 0 0 Seq.empty vars (Set.fromList ["Int", "Bool"])) >>= print)
            ((\err -> putStrLn (unpack $ ppErrorReport $ ErrReport (pack file) (decodeUtf8 str) err)) :: TypeError -> IO ())
    Left err -> putStrLn (unpack $ ppErrorReport $ ErrReport (pack file) (decodeUtf8 str) err)
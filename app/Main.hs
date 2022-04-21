module Main (main) where

import Typer.Env
import Data.Text            (pack, unpack, Text)
import Data.Text.Encoding   (decodeUtf8)
import GHC.IO.Encoding      (setLocaleEncoding, utf8)
import System.Environment   (getArgs)
import Error.PrettyPrint    (ppErrorReport)
import Error.Message        (ErrReport (ErrReport))
import Syntax.Lexer.Support (runLexer)
import Typer.Infer          (processProgram)
import Typer.Error          (TypeError)
import Control.Exception    (catch)
import Data.Map             (Map)
import Syntax.Parser        (parseProgram)

import qualified Data.ByteString as SB
import qualified Data.Map        as Map
import qualified Data.Sequence   as Seq

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

ctx :: Env
ctx =
  Env 0
      0
      Seq.empty vars (Map.fromList [("Int", TyNamed Nothing "Int"), ("Bool", TyNamed Nothing "Bool")])
      (Map.empty)
      0

main :: IO ()
main = do
  setLocaleEncoding utf8
  [file] <- getArgs
  str <- SB.readFile file
  case runLexer parseProgram str  of
    Right res -> do
      catch (do
              (ty, ctx') <- runEnv (processProgram res) ctx
              putStrLn ("Res: " ++ show ty)
              print (ctx'.dataCons))
            ((\err -> putStrLn (unpack $ ppErrorReport $ ErrReport (pack file) (decodeUtf8 str) err)) :: TypeError -> IO ())
    Left err -> putStrLn $ "Res:2 " ++ (unpack $ ppErrorReport $ ErrReport (pack file) (decodeUtf8 str) err)
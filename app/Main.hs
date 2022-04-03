module Main (main) where

import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Pretty.Tree (drawTree)
import System.Environment (getArgs)
import GHC.IO (catch)

import Error.PrettyPrint (ppErrorReport)
import Error.Message (ErrReport (ErrReport))

import Syntax.Parser ( parseType ) 
import Syntax.Lexer.Support (runLexer)

import Typer.Context ( Ctx(Ctx) )
import Typer.Types ( Kind(Star, KFun, KGen), KindScheme(KindScheme), Loc (Ghost) )
import Typer.Kinds (infer)
import Typer.Errors ( TypeError )
import Typer.Tracer (emptyTracer)

import qualified Data.ByteString as SB
import qualified Syntax.Range as Range
import qualified Data.Map as Map
import qualified Control.Monad.State as State

star = Star Ghost

ctx :: Ctx
ctx = Ctx 0 0 Map.empty
              Map.empty
              (Map.fromList [ ("Int", KindScheme 0 star)
                            , ("List", KindScheme 0 (KFun Ghost star star))
                            , ("Either", KindScheme 2 (KFun Ghost (KGen Ghost 0) (KFun Ghost (KGen Ghost 1) star)))])
              Range.empty



main :: IO ()
main = do
  setLocaleEncoding utf8
  [file] <- getArgs
  str <- SB.readFile file
  case runLexer parseType str of
    Right res -> do 
      putStrLn (drawTree res)
      catch (State.evalStateT (infer ctx res) emptyTracer >>= print) 
            ((\err -> putStrLn (unpack $ ppErrorReport $ ErrReport (pack file) (decodeUtf8 str) err)) :: TypeError -> IO ())
    Left err -> putStrLn (unpack $ ppErrorReport $ ErrReport (pack file) (decodeUtf8 str) err) 
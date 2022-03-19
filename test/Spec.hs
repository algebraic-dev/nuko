import Test.Tasty
import Test.Tasty.Golden (findByExtension)
import Test.Tasty.HUnit

import System.FilePath (dropExtension, addExtension)
import Data.Traversable (for)
import Data.Either (isRight)

import Syntax.Lexer.Support
import Syntax.Parser

import qualified Data.ByteString as SB
import qualified Data.Text as Text

import Error.PrettyPrint (ppErrorReport)
import Error.Message (ErrReport(ErrReport))
import Data.Text.Encoding (decodeUtf8)
import GHC.IO.Exception (assertError)

excp :: SB.ByteString -> FilePath -> IO ()
excp content file =
  case runLexer parseProgram content of
    Right _ -> pure ()
    Left err -> error ("\n" ++ Text.unpack (ppErrorReport $ ErrReport (Text.pack file) (decodeUtf8 content) err))

runFile :: FilePath -> IO TestTree
runFile file = do
  content <- SB.readFile $ addExtension file ".nk"
  pure $ testCase ("Parsing '" ++ file ++ "'") $ assertError True (excp content file)

main :: IO ()
main = do
    filesNoExt <- map dropExtension <$> findByExtension [".nk"] "test/suite"
    tests <- for filesNoExt runFile
    defaultMain $ testGroup "Parsing files" tests

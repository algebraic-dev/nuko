import Test.Tasty
import Test.Tasty.Golden             (findByExtension, goldenVsString)
import System.FilePath               (dropExtension, addExtension)
import Data.Traversable              (for)
import Data.ByteString.Lazy.Internal (packChars)
import Text.Pretty.Simple            (pShowNoColor)

import Nuko.Syntax.Lexer.Support
import Nuko.Syntax.Lexer         (scan)
import Nuko.Syntax.Lexer.Tokens  (Token(TcEOF))
import Nuko.Syntax.Range         (Ranged (info))
import Nuko.Syntax.Parser        (parseProgram)

import qualified Data.ByteString as ByteString
import qualified Data.Text.Lazy as Text

goldenStr :: String -> String -> String -> TestTree
goldenStr file golden str = goldenVsString file golden (pure (packChars str))

-- Lexer tests

scanUntilEnd :: Lexer [Ranged Token]
scanUntilEnd = do
  res <- scan
  case res.info of
    TcEOF -> pure [res]
    _     -> (res :) <$> scanUntilEnd

runFile :: FilePath -> IO TestTree
runFile file = do
  content <- ByteString.readFile $ addExtension file ".nk"
  let golden = addExtension file ".golden"
  pure $ either
    (goldenStr file golden . show)
    (goldenStr file golden . (unlines . map show))
    (runLexer scanUntilEnd content)

runParser :: FilePath -> IO TestTree
runParser file = do
  content <- ByteString.readFile $ addExtension file ".nk"
  let golden = addExtension file ".golden"
  pure $ either
    (goldenStr file golden . show)
    (goldenStr file golden . Text.unpack . pShowNoColor)
    (runLexer parseProgram content)

runTestPath :: TestName -> FilePath -> (FilePath -> IO TestTree) -> IO TestTree
runTestPath name path run = do
  filesNoExt <- map dropExtension <$> findByExtension [".nk"] path
  tests <- for filesNoExt run
  pure (Test.Tasty.testGroup name tests)

main :: IO ()
main = do
  testTree <- sequence 
    [ runTestPath "Lexing" "tests/lexer" runFile
    , runTestPath "Parsing" "tests/parser" runParser
    ]
  defaultMain $ Test.Tasty.testGroup "Tests" testTree
import Test.Tasty
import Test.Tasty.Golden (findByExtension, goldenVsString)
import System.FilePath   (dropExtension, addExtension)
import Data.Traversable  (for)
import Data.ByteString.Lazy.Internal (packChars)
import Data.Text.Encoding (decodeUtf8)

import Nuko.Syntax.Lexer.Support
import Nuko.Syntax.Lexer         (scan)
import Nuko.Syntax.Lexer.Tokens  (Token(TcEOF))
import Nuko.Syntax.Range         (Ranged (info))
import Nuko.Error.Data           (CompilerError(..), Report (Report), emptyReport)
import Nuko.Error.Render         (prettyPrint)

import qualified Data.ByteString as ByteString
import qualified Data.Text       as Text

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

runTestPath :: TestName -> FilePath -> (FilePath -> IO TestTree) -> IO TestTree
runTestPath name path run = do
  filesNoExt <- map dropExtension <$> findByExtension [".nk"] path
  tests <- for filesNoExt run
  pure (Test.Tasty.testGroup name tests)

main :: IO ()
main = do
  testTree <- sequence 
    [ runTestPath "Lexing" "tests/lexer" runFile
    ]
  defaultMain $ Test.Tasty.testGroup "Tests" testTree
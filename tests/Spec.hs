import Relude
import Data.These

import Test.Tasty
import Test.Tasty.Golden             (findByExtension, goldenVsString)
import System.FilePath               (dropExtension, addExtension)

import Nuko.Resolver                 (resolveProgram)
import Nuko.Syntax.Lexer.Support     (runLexer, Lexer)
import Nuko.Syntax.Lexer             (scan)
import Nuko.Syntax.Lexer.Tokens      (Token(TcEOF))
import Nuko.Syntax.Range             (Ranged (info))
import Nuko.Syntax.Parser            (parseProgram)

import Text.Pretty.Simple            (pShowNoColor)

import Resolver.PreludeImporter      (runResolverNull)

import qualified Data.ByteString as ByteString
import qualified Data.Text.Lazy as LazyT

scanUntilEnd :: Lexer [Ranged Token]
scanUntilEnd = do
  res <- scan
  case res.info of
    TcEOF -> pure [res]
    _     -> (res :) <$> scanUntilEnd

goldenStr :: FilePath -> FilePath -> Text -> TestTree
goldenStr file golden str = goldenVsString file golden (pure $ encodeUtf8 str)

prettyShow :: Show a => a -> Text
prettyShow item = LazyT.toStrict $ pShowNoColor item

stringifyErr :: Show a => These [a] b -> These Text b
stringifyErr (That e) = That e
stringifyErr (This e) = This (unlines $ map prettyShow e)
stringifyErr (These e f) = These (unlines $ map prettyShow e) f

runFile :: FilePath -> IO TestTree
runFile file = do
  content <- ByteString.readFile $ addExtension file ".nk"
  let golden = addExtension file ".golden"
  pure $ case runLexer scanUntilEnd content of
    That e    -> goldenStr file golden ("That " <> unlines (map prettyShow e))
    This e    -> goldenStr file golden ("This " <> unlines (map prettyShow e))
    These e f -> goldenStr file golden ("These " <> unlines (map prettyShow e) <> "\n" <> unlines (map prettyShow f))

runParser :: FilePath -> IO TestTree
runParser file = do
  content <- ByteString.readFile $ addExtension file ".nk"
  let golden = addExtension file ".golden"
  pure $ case runLexer parseProgram content of
    That e    -> goldenStr file golden ("That " <> prettyShow e)
    This e    -> goldenStr file golden ("This " <> unlines (map prettyShow e))
    These e f -> goldenStr file golden ("These " <> unlines (map prettyShow e) <> "\n" <> prettyShow f)

runResolver :: FilePath -> IO TestTree
runResolver file = do
  content <- ByteString.readFile $ addExtension file ".nk"
  let golden = addExtension file ".golden"
  pure $ case stringifyErr (runLexer parseProgram content) >>= \x -> stringifyErr (runResolverNull (resolveProgram x)) of
    That e    -> goldenStr file golden ("That " <> prettyShow e)
    This e    -> goldenStr file golden ("This " <> e)
    These e f -> goldenStr file golden ("These " <> e <> "\n" <> prettyShow f)

runTestPath :: TestName -> FilePath -> (FilePath -> IO TestTree) -> IO TestTree
runTestPath name path run = do
  filesNoExt <- map dropExtension <$> findByExtension [".nk"] path
  tests <- traverse run filesNoExt
  pure (Test.Tasty.testGroup name tests)

main :: IO ()
main = do
  testTree <- sequence
    [ runTestPath "Lexing" "tests/lexer" runFile
    , runTestPath "Parsing" "tests/parser" runParser
    , runTestPath "Parsing" "tests/resolver" runResolver ]
  defaultMain $ Test.Tasty.testGroup "Tests" testTree
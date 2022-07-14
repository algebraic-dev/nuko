import Relude                      (id, ($), Show, IO, unlines, FilePath, ConvertUtf8(encodeUtf8), ByteString, (.), fst, Bool (..), HashMap, zip, Applicative ((<*)), uncurry, traverse_)
import Relude.String               (Text)
import Relude.Monoid               (Semigroup((<>)),Endo(appEndo))
import Relude.Monad                (Monad((>>=)), MonadIO, Maybe(..))
import Relude.Functor              (fmap, (<$>), first)
import Relude.Foldable             (Traversable(sequence, traverse), forM, for_)
import Relude.Applicative          (Applicative(pure))
import Data.These                  (These(..))
import Test.Tasty                  (defaultMain, testGroup, TestName, TestTree)
import Test.Tasty.Golden           (findByExtension, goldenVsString)
import System.FilePath             (dropExtension, addExtension)
import Data.Traversable            (for)
import Nuko.Syntax.Lexer.Support   (runLexer, Lexer)
import Nuko.Syntax.Lexer.Tokens    (Token(TcEOF))
import Nuko.Syntax.Lexer           (scan)
import Nuko.Syntax.Parser          (parseProgram)
import Nuko.Report.Range           (Ranged (info))
import Text.Pretty.Simple          (pShowNoColor)
import Pretty.Tree                 (PrettyTree(prettyShowTree))
import Resolver.PreludeImporter
import Nuko.Tree
import Data.Aeson.Text
import Data.Aeson
import Data.Text.Lazy (toStrict)

import qualified Data.HashMap.Strict           as HashMap
import qualified Data.ByteString               as ByteString
import qualified Data.Text.Lazy                as LazyT
import qualified Control.Monad.State.Strict    as State
import qualified Control.Monad.Trans.Chronicle as Chronicle
import qualified Control.Monad.Reader          as Reader

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

stringifyErr :: Show a => These [a] b -> These [Text] b
stringifyErr (That e) = That e
stringifyErr (This e) = This (fmap prettyShow e)
stringifyErr (These e f) = These (fmap prettyShow e) f

toStrictText :: ToJSON a => a -> Text
toStrictText = toStrict . encodeToLazyText

treeErr :: ToJSON a => These [a] b -> These [Text] b
treeErr = first (fmap toStrictText)

formatErr :: ToJSON a => These a b -> These Text b
formatErr = first toStrictText

runThat :: (b -> Text) ->  (forall m. MonadIO m => ByteString -> m (These [Text] b)) -> FilePath -> IO TestTree
runThat fn that file = do
  content <- ByteString.readFile $ addExtension file ".nk"
  let golden = addExtension file ".golden"
  resThat <- that content
  pure $ goldenStr file golden $ case resThat of
    That e    -> "✓ That\n"  <> fn e
    This e    -> "✗ This\n"  <> unlines e
    These e f -> "• These\n" <> unlines e <> "\n" <> fn f

runFile :: FilePath -> IO TestTree
runFile = runThat prettyShowTree (pure . treeErr . runLexer scanUntilEnd)

runParser :: FilePath -> IO TestTree
runParser = runThat prettyShowTree (pure . treeErr . runLexer parseProgram)

runTestPath :: TestName -> FilePath -> (FilePath -> IO TestTree) -> IO TestTree
runTestPath name path run = do
  filesNoExt <- fmap dropExtension <$> findByExtension [".nk"] path
  tests <- traverse run filesNoExt
  pure (Test.Tasty.testGroup name tests)


runResolver :: FilePath -> IO TestTree
runResolver = runThat prettyShowTree $ \content -> pure (stringifyErr (runLexer parseProgram content) >>= \ast -> treeErr (resolveEntireProgram ast))

main :: IO ()
main = do
  testTree <- sequence
    [ runTestPath "Lexing" "tests/Suite/lexer" runFile
    , runTestPath "Parsing" "tests/Suite/parser" runParser
    , runTestPath "Resolving" "tests/Suite/resolver" runResolver]
  defaultMain $ Test.Tasty.testGroup "Tests" testTree
import Relude                      (id, ($), Show, IO, unlines, FilePath, ConvertUtf8(encodeUtf8), ByteString, (.), fst, Bool (..), HashMap, zip, Applicative ((<*)), uncurry)
import Relude.String               (Text)
import Relude.Monoid               (Semigroup((<>)),Endo(appEndo))
import Relude.Monad                (Monad((>>=)), MonadIO, Maybe(..))
import Relude.Functor              (fmap, (<$>), first)
import Relude.Foldable             (Traversable(sequence, traverse), forM)
import Relude.Applicative          (Applicative(pure))

import Data.These                  (These(..))

import Test.Tasty                  (defaultMain, testGroup, TestName, TestTree)
import Test.Tasty.Golden           (findByExtension, goldenVsString)
import System.FilePath             (dropExtension, addExtension)

import Nuko.Syntax.Lexer.Support   (runLexer, Lexer)
import Nuko.Syntax.Lexer           (scan)
import Nuko.Syntax.Lexer.Tokens    (Token(TcEOF))
import Nuko.Report.Range           (Ranged (info))
import Nuko.Syntax.Parser          (parseProgram)
import Nuko.Typer.Env              (updateTyKind, MonadTyper, TypeSpace(..), emptyTS, TyInfo(..), TypingEnv (TypingEnv), addTyKind)
import Nuko.Typer.Infer            (initTypeDecl, inferTypeDecl, checkTypeSymLoop)
import Nuko.Typer.Error            (TypeError)
import Nuko.Typer.Types            (TKind(..), fixKindHoles, removeStar)
import Nuko.Typer.Infer.TypeDecl   (InitTypeData(..))

import Text.Pretty.Simple          (pShowNoColor)

import Resolver.PreludeImporter    (resolveEntireProgram)
import Pretty.Tree                 (PrettyTree(prettyShowTree))

import Nuko.Tree

import qualified Data.HashMap.Strict           as HashMap
import qualified Data.ByteString               as ByteString
import qualified Data.Text.Lazy                as LazyT
import qualified Control.Monad.State.Strict    as State
import qualified Control.Monad.Trans.Chronicle as Chronicle
import qualified Control.Monad.Reader          as Reader
import Data.Traversable (for)

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

runThat :: Show a =>  (b -> Text) ->  (forall m. MonadIO m => ByteString -> m (These [a] b)) -> FilePath -> IO TestTree
runThat fn that file = do
  content <- ByteString.readFile $ addExtension file ".nk"
  let golden = addExtension file ".golden"
  resThat <- that content
  pure $ goldenStr file golden $ case resThat of
    That e    -> "✓ That\n"  <> fn e
    This e    -> "✗ This\n"  <> unlines (fmap prettyShow e)
    These e f -> "• These\n" <> unlines (fmap prettyShow e) <> "\n" <> fn f

runFile :: FilePath -> IO TestTree
runFile = runThat prettyShowTree (pure . runLexer scanUntilEnd)

runParser :: FilePath -> IO TestTree
runParser = runThat prettyShowTree (pure . runLexer parseProgram)

runResolver :: FilePath -> IO TestTree
runResolver = runThat prettyShowTree $ \content -> pure (stringifyErr (runLexer parseProgram content) >>= \ast -> stringifyErr (resolveEntireProgram ast))

runTypeChecker :: MonadIO m => TypeSpace -> (forall m . MonadTyper m => m a) -> m (These [TypeError] (a, TypingEnv))
runTypeChecker ts act =
  let te = TypingEnv "Main" ts in
  first (`appEndo` []) <$> (Chronicle.runChronicleT (State.runStateT act te))

runTyper :: FilePath -> IO TestTree
runTyper = runThat prettyShowTree $ \content -> do
    let result = stringifyErr (runLexer parseProgram content) >>= \ast -> stringifyErr (resolveEntireProgram ast)
    let ts = emptyTS { _tsTypes = HashMap.fromList [("Prelude.Int", (KiStar, IsTyDef)), ("Prelude.String", (KiStar, IsTyDef))] }
    case result of
      This a    -> pure (This (fmap prettyShow a))
      That a    -> stringifyErr <$> runTypeChecker ts (run (fst a))
      These _ a -> stringifyErr <$> runTypeChecker ts (run (fst a))
  where
    run :: MonadTyper m => Program Re -> m [TypeDecl Tc]
    run program = do
      initDatas <- traverse initTypeDecl program.typeDecls
      checkTypeSymLoop program.typeDecls
      checkedTc <- traverse (uncurry inferTypeDecl) (zip program.typeDecls initDatas)

      for initDatas $ \initData -> do
        res <- fixKindHoles initData.itResKind >>= removeStar
        updateTyKind initData.itCanonName (\(_, i) -> Just (res, i))

      pure checkedTc

runTestPath :: TestName -> FilePath -> (FilePath -> IO TestTree) -> IO TestTree
runTestPath name path run = do
  filesNoExt <- fmap dropExtension <$> findByExtension [".nk"] path
  tests <- traverse run filesNoExt
  pure (Test.Tasty.testGroup name tests)

main :: IO ()
main = do
  testTree <- sequence
    [ runTestPath "Lexing" "tests/Suite/lexer" runFile
    , runTestPath "Parsing" "tests/Suite/parser" runParser
    , runTestPath "Resolver" "tests/Suite/resolver" runResolver
    , runTestPath "Typer" "tests/Suite/typer" runTyper ]
  defaultMain $ Test.Tasty.testGroup "Tests" testTree
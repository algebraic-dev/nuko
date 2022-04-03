import Test.Tasty
import Typer.Types
import Test.Tasty.HUnit
import Test.Tasty.Golden (findByExtension)
import System.FilePath (dropExtension, addExtension)
import Data.Traversable (for)
import Typer.Context (Ctx (Ctx))

import Syntax.Lexer.Support
import Syntax.Parser ( parseProgram, parseType )
import Error.PrettyPrint (ppErrorReport)
import Error.Message (ErrReport(ErrReport))
import Data.Text.Encoding (decodeUtf8)
import GHC.IO.Exception (assertError)
import Data.ByteString (ByteString)

import qualified Data.ByteString as SB
import qualified Data.Text as Text
import qualified Syntax.Range as Range
import qualified Data.Map as Map
import Typer.Kinds (infer)
import qualified Control.Monad.State as State
import qualified Control.Applicative as Seq
import Typer.Tracer (TyperState(TyperState))


toError :: ByteString -> FilePath -> ErrKind -> String
toError content file err = "\n" ++ Text.unpack (ppErrorReport $ ErrReport (Text.pack file) (decodeUtf8 content) err)

whenLeft :: Applicative m => Either e a -> (e -> m ()) -> m ()
whenLeft (Left e) f   = f e
whenLeft (Right _) _ = pure ()

runFile :: FilePath -> IO TestTree
runFile file = do
  content <- SB.readFile $ addExtension file ".nk"
  pure $ testCase ("Parsing '" ++ file ++ "'")
       $ assertError True
       $ whenLeft (runLexer parseProgram content) (error . toError content file)

ctx :: Ctx
ctx = Ctx 0 0 Map.empty
              Map.empty
              (Map.fromList [ ("Int", KindScheme 0 Star)
                            , ("List", KindScheme 0 (KFun Star Star))
                            , ("Either", KindScheme 2 (KFun (KGen 0) (KFun (KGen 1) Star)))])
              Range.empty



runKindUnification :: FilePath -> IO TestTree
runKindUnification file = do
    content <- SB.readFile $ addExtension file ".nk"
    golden  <- readFile $ addExtension file ".golden"
    pure $ testCase ("Infer '" ++ file ++ "'")
        $ assertError True
        $ either (error . toError content file) (kindRun golden) (runLexer parseType content)
  where 
    kindRun golden ty = do
      (_, k) <- State.evalStateT (infer ctx ty) (TyperState Seq.empty) 
      show k @?= golden
      pure ()

runTestPath :: TestName -> FilePath -> (FilePath -> IO TestTree) -> IO TestTree
runTestPath name path run = do
  filesNoExt <- map dropExtension <$> findByExtension [".nk"] path
  tests <- for filesNoExt run
  pure (Test.Tasty.testGroup name tests)



main :: IO ()
main = do
  let tests = [ runTestPath "SuccessInferKind" "test/suite/kinds/success" runKindUnification
              , runTestPath "SuccessParse" "test/suite/functions" runFile
              ]
  testTree <- sequence tests
  defaultMain $ Test.Tasty.testGroup "Tests" testTree

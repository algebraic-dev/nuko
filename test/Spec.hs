import Test.Tasty
import Test.Tasty.Golden (findByExtension)
import Test.Tasty.HUnit

import System.FilePath (dropExtension, addExtension)
import Data.Traversable (for)
import Data.Either (isRight)

import Syntax.Lexer.Support
import Syntax.Parser

import qualified Data.ByteString as SB

runFile :: FilePath -> IO TestTree
runFile file = do 
  content <- SB.readFile $ addExtension file ".nk"
  pure $ testCase ("Parsing '" ++ file ++ "'") $
     assertEqual ("The file '" ++ file ++ "' will be parsed!")
               True
               (isRight (runLexer parseProgram content))
 
main :: IO ()
main = do 
    filesNoExt <- map dropExtension <$> findByExtension [".nk"] "test/suite"
    tests <- for filesNoExt runFile
    defaultMain $ testGroup "Parsing files" tests

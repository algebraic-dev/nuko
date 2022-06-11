module Main (main) where

import Relude
import Nuko.Syntax.Lexer.Support (runLexer)
import Nuko.Syntax.Lexer (scan)

main :: IO ()
main = print $ runLexer (replicateM 3 scan) "a ? e"
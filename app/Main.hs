module Main where

import Syntax.Lexer 
import Syntax.Lexer.Support
import System.Environment
import Syntax.Parser

import qualified Data.ByteString as SB

main :: IO ()
main = do 
    [file] <- getArgs
    bs     <- SB.readFile file 
    print "a"
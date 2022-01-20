module Main where 

import Error.Message
import Error.PrettyPrint
import Syntax.Bounds

import Data.Text (unpack)
import GHC.IO.Encoding

main :: IO ()
main = do  
  setLocaleEncoding utf8
  putStrLn 
    $ unpack 
    $ ppShow 
    $ ErrReport "./hello-world.nk" "some error here" (UnfinishedString (Pos 1 2))

{

module Nuko.Syntax.Lexer where

import Data.Text            (Text, append, index)
import Data.Text.Encoding   (decodeUtf8)
import Data.Text.Read       (decimal, double)
import Control.Monad        (when)

import Nuko.Syntax.Lexer.Support
import Nuko.Syntax.Lexer.Tokens
import Nuko.Syntax.Range

import qualified Control.Monad.State  as State
import qualified Control.Monad.Except as Error
import qualified Data.ByteString      as ByteString

}

$newline = [\n\r]
$space   = [\t\v\f\ ]
$digit   = [0-9]
$lower   = [a-z]
$upper   = [A-Z]
$letter  = [a-zA-Z]
$symbol  = [\+\-\*\/\<\>\=\^\?]
$end     = [\?\!]
$graphic = [\x21-\x7E]

@id_char   = $letter | $digit | _
@lower_id  = $lower @id_char* $end?
@upper_id  = $upper @id_char* $end?
@wild      = _
@number    = $digit+
@stringraw = [$graphic$space]|$newline

lexer :-

<0> $space+    ;
<0> $newline+  { \_ _ -> undefined }

{

-- Probably will not throw errors because all the data is determined
-- by the lexer

fromRight :: Either e r -> r
fromRight (Right b) = b
fromRight _ = error "Cannot unpack the data (error on lexer UwU)"

scan :: Lexer (Ranged Token)
scan = do
    inputCode <- State.gets input
    code <- startCode
    case alexScan inputCode code of
        AlexEOF -> pure undefined
        AlexError inp -> undefined
        AlexSkip restInput _ -> undefined
        AlexToken input' tokl action -> undefined

}

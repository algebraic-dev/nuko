{

module Syntax.Lexer where

import Syntax.Lexer.Support

import qualified Control.Monad.State as ST
import qualified Control.Monad.Except as ER
import qualified Data.ByteString as BS
import Syntax.Bounds
import Data.Text (Text, append)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Read (decimal)

import Syntax.Lexer.Tokens

}

%encoding "latin1"

$newline = [\n\r]
$space = [\t\v\f\ ]
$digit = [0-9]
$lower = [a-z]
$upper = [A-Z]
$letter = [a-zA-Z]
$symbol = [\+\-\*\/\<\>\=\^\?]
$end = [\?\!]

@id_char = $letter | $digit | _ 
@lower_id = $lower @id_char* $end?
@upper_id = $upper @id_char* $end?
@number   = $digit+

program :- 
    
<0> $space+    ;
<0> $newline+  { \_ -> pushCode newline *> rawScan }

<0> "let"     { token TknKwLet   }
<0> "type"    { token TknKwType  }
<0> "match"   { token TknKwMatch }
<0> "if"      { token TknKwIf }
<0> "then"    { token TknKwThen }
<0> "else"    { token TknKwElse }

<0> "do"       { layoutKw TknKwDo }
<0> "with"     { layoutKw TknKwWith }

<0> @number    { emit (TknNumber . fst . fromRight . decimal) }
<0> @lower_id  { emit TknLowerId  }
<0> @upper_id  { emit TknUpperId  }

<0> "("        { token TknLPar   }
<0> ")"        { token TknRPar   }
<0> "{"        { token TknLBrace }
<0> "}"        { token TknRBrace }
<0> "="        { token TknEq     }
<0> ":"        { token TknColon  }
<0> "|"        { token TknPipe   }
<0> ","        { token TknComma   }
<0> "\"        { token TknSlash   }
<0> "->"       { token TknRArrow   }

<0> $symbol+   { emit TknSymbol  }
<0> "--"       { \_ -> pushCode linecom *> rawScan }
<0> \"         { \_ -> pushCode str *> rawScan }

-- Layout Parsing

<layout> $space+    ;
<layout> $newline+  ;
<layout> ()         { startLayout }

<newline> $space+   ;
<newline> $newline  ;
<newline> () { offsideRule }

<empty_layout> () { emptyLayout }

-- Line comments

<linecom> [^\n] ;
<linecom> \n    { \_ ->  popCode *> rawScan }

-- Strings 

<str> \"   { \_ -> popCode *> (TknLStr <$> resetBuffer) }
<str> [^\"] { \c -> addToBuffer c *> rawScan }

{

fromRight :: Either e r -> r 
fromRight (Right b) = b
fromRight _ = error "Cannot unpack the data (error on lexer UwU)"

addToBuffer :: Text -> Lexer ()
addToBuffer c = ST.modify (\s -> s { lsBuffer = append (lsBuffer s) c })

resetBuffer :: Lexer Text
resetBuffer = ST.state (\s -> (lsBuffer s, s { lsBuffer = "" }))

emptyLayout _ = replaceCode newline *> pure TknClose

handleEOF = lastLayout >>= \case 
    Nothing -> popCode *> pure (WithBounds TknEOF empty) 
    Just _  -> popLayout *> pure (WithBounds TknClose empty)

offsideRule _ = do 
    lay <- lastLayout
    col <- ST.gets (column . inputPos . lsInput)
    let continue = popCode *> rawScan 
    case lay of 
        Nothing   -> continue
        Just col' -> do 
            case col `compare` col' of
                EQ -> popCode *> pure TknEnd 
                GT -> continue 
                LT -> popLayout *> pure TknClose

startLayout _ = do  
    popCode 
    ref <- lastLayout 
    col <- ST.gets (column . inputPos . lsInput)
    if Just col <= ref 
        then pushCode empty_layout
        else pushLayout col
    pure TknOpen

layoutKw t _ = pushCode layout *> pure t

rawScan = info <$> scan

scan :: Lexer (WithBounds Token)
scan = do 
    input <- ST.gets lsInput 
    code <- startCode 
    case alexScan input code of 
        AlexEOF -> handleEOF
        AlexError inp -> ER.throwError $ "Error on lexicon " ++ show (inputPos inp)
        AlexSkip input' _ -> ST.modify (\s -> s { lsInput = upPos input' }) *> scan 
        AlexToken input' tokl action -> do
            ST.modify (\s -> s { lsInput = input' })
            lastPos <- updateLastPos *> ST.gets (inputLastPos . lsInput)
            res     <- action (decodeUtf8 $ BS.take tokl (inputStream input))
            input''  <- ST.gets (lsInput)
            pure (WithBounds res (Bounds lastPos (inputPos input'')))

}
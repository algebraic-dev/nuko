{

module Syntax.Lexer where


import Data.Text            (Text, append, index)
import Data.Text.Encoding   (decodeUtf8)
import Data.Text.Read       (decimal, double)
import Syntax.Lexer.Support
import Syntax.Bounds
import Syntax.Lexer.Tokens

import qualified Control.Monad.State as ST
import qualified Control.Monad.Except as ER
import qualified Data.ByteString as BS
import qualified Error.Message as ERR

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
@wild     = _
@number   = $digit+

program :- 
    
<0> $space+    ;
<0> $newline+  { \_ _ -> pushCode newline *> scan }

<0> "let"      { token TknKwLet   }
<0> "type"     { token TknKwType  }
<0> "match"    { token TknKwMatch }
<0> "if"       { token TknKwIf }
<0> "then"     { token TknKwThen }
<0> "else"     { token TknKwElse }
<0> "import"   { token TknKwImport }
<0> "do"       { layoutKw TknKwDo }
<0> "as"       { token TknKwAs }
<0> "external" { token TknKwExternal }

<0> "with"     { layoutKw TknKwWith }

<0> "'" [^\'] "'" { emit $ TknLChar . (`Data.Text.index` 1) }
<0> @number "." @number { emit (TknLDouble . fst . fromRight . double)}
<0> @wild      { token TknWild }
<0> @number    { emit (TknNumber . fst . fromRight . decimal) }
<0> @lower_id  { emit TknLowerId  }
<0> @upper_id  { emit TknUpperId  }

<0> "("        { token TknLPar   }
<0> ")"        { token TknRPar   }
<0> "{"        { token TknLBrace }
<0> "}"        { token TknRBrace }
<0> "="        { token TknEq  }
<0> ":"        { token TknColon  }
<0> "|"        { token TknPipe   }
<0> ","        { token TknComma   }
<0> "."        { token TknDot   }
<0> "\"        { token TknSlash   }
<0> "->"       { token TknRArrow   }

<0> $symbol+   { emit TknSymbol  }
<0> "--"       { \_ _   -> pushCode linecom *> scan }
<0> \"         { \_ pos -> pushCode str *> scan >>= \res ->
                           pure (WithBounds (info res) (Bounds pos (end $ position res)))}

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
<linecom> \n    { \_ _ ->  popCode *> scan }

-- Strings 

<str> \"   { \_ p -> popCode *> resetBuffer >>= \s -> emit TknLStr s p  }
<str> [^\"] { \c _ -> addToBuffer c *> scan }

{

ghostBounds :: t -> Lexer (WithBounds t) 
ghostBounds t = do 
    pos <- ST.gets (inputPos . lsInput)
    pure $ WithBounds t (Bounds pos pos)

fromRight :: Either e r -> r 
fromRight (Right b) = b
fromRight _ = error "Cannot unpack the data (error on lexer UwU)"

addToBuffer :: Text -> Lexer ()
addToBuffer c = ST.modify (\s -> s { lsBuffer = append (lsBuffer s) c })

resetBuffer :: Lexer Text
resetBuffer = ST.state (\s -> (lsBuffer s, s { lsBuffer = "" }))

emptyLayout _ _ = replaceCode newline *> ghostBounds TknClose

handleEOF = lastLayout >>= \case 
    Nothing -> popCode *> ghostBounds TknEOF
    Just _  -> popLayout *> ghostBounds TknClose

offsideRule _ _ = do 
    lay <- lastLayout
    col <- ST.gets (column . inputPos . lsInput)
    let continue = popCode *> scan 
    case lay of 
        Nothing   -> continue
        Just col' -> do 
            case col `compare` col' of
                EQ -> popCode *> ghostBounds TknEnd
                GT -> continue 
                LT -> popLayout *> ghostBounds TknClose

startLayout _ _ = do  
    popCode 
    ref <- lastLayout 
    col <- ST.gets (column . inputPos . lsInput)
    if Just col <= ref 
        then pushCode empty_layout
        else pushLayout col
    ghostBounds TknOpen

layoutKw t text pos = pushCode layout *> token t text pos

scan :: Lexer (WithBounds Token)
scan = do 
    input <- ST.gets lsInput 
    code <- startCode 
    case alexScan input code of 
        AlexEOF -> handleEOF
        AlexError inp -> ER.throwError $ ERR.UnrecognizableChar (inputPos inp)
        AlexSkip input' _ -> ST.modify (\s -> s { lsInput = input' }) >> scan 
        AlexToken input' tokl action -> do  
            pos <- ST.gets (inputPos . lsInput)
            ST.modify (\s -> s { lsInput = input' })
            res      <- action (decodeUtf8 $ BS.take tokl (inputStream input)) pos
            pure res
}
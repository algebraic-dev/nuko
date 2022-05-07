
{

module Nuko.Syntax.Lexer where

import Data.Text            (Text, append, index)
import Data.Text.Encoding   (decodeUtf8)
import Data.Text.Read       (decimal, double)
import Control.Monad        (when)

import Nuko.Syntax.Lexer.Support
import Nuko.Syntax.Lexer.Tokens
import Nuko.Syntax.Range
import Nuko.Syntax.Error

import Debug.Trace

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

<0> $space+     ;
<0> $newline+   { \_ _ -> pushCode newline *> scan }

<0> "--"        { \_ _   -> pushCode linecom *> scan }

<linecom> [^\n] ;
<linecom> \n    { \_ _ ->  popCode *> scan }
<linecom> eof   { \_ _ ->  popCode *> scan }

<0> "type"      { nonLWToken TcType  }
<0> "with"      { layoutKw TcWith }
<0> "let"       { token TcLet }
<0> "match"     { token TcMatch }
<0> "if"        { token TcIf }
<0> "then"      { token TcThen }
<0> "else"      { token TcElse }
<0> "forall"    { token TcForall }
<0> "pub"       { token TcPub }
<0> \"          { \_ pos -> do
                    pushCode str
                    res <- scan
                    pure (Ranged (info res) (Range pos (end $ position res)))
                }

<str> \"        { \_ pos  -> popCode *> resetBuffer >>= \s -> emit TcStr s pos  }
<str> .         { \char _ -> addToBuffer char *> scan }

<0> @lower_id   { emit TcLowerId  }
<0> @upper_id   { emit TcUpperId  }
<0> @number     { emit $ TcInt . fst . fromRight . decimal }

<0> "="         { layoutKw TcEqual }
<0> "("         { token TcLPar   }
<0> ")"         { token TcRPar   }
<0> "{"         { token TcLBrace }
<0> "}"         { token TcRBrace }
<0> ":"         { token TcColon  }
<0> "|"         { token TcPipe   }
<0> "."         { token TcDot    }
<0> "\\"        { token TcSlash  }
<0> "->"        { token TcArrow  }
<0> "=>"        { token TcDoubleArrow  }

-- Rule for parsing layout
<layout_> $space+    ;
<layout_> $newline+  ;
<layout_> "--"       { \_ _   -> pushCode linecom *> scan }
<layout_> ()         { startLayout }

<empty_layout> () { emptyLayout }

<newline> $space+   ;
<newline> $newline  ;
<newline> () { offsideRule }

{

-- Some layout thing helpers

emptyLayout _ _ = popCode *> pushCode newline *> ghostRange TcEnd

layoutKw t text pos = do
    isAfterNonLayoutKW <- State.gets nonLw
    if isAfterNonLayoutKW
        then State.modify (\s -> s { nonLw = False })
        else pushCode layout_
    token t text pos

startLayout _ _ = do
    popCode
    ref <- lastLayout
    col <- State.gets (column . currentPos . input)
    if Just col <= ref
        then pushCode empty_layout
        else pushLayout col
    ghostRange TcBegin

offsideRule _ _ = do
    lay <- lastLayout
    col <- State.gets (column . currentPos . input)
    let continue = popCode *> scan
    case lay of
        Nothing   -> continue
        Just col' -> do
            case col `compare` col' of
                EQ -> popCode *> ghostRange TcSep
                GT -> continue
                LT -> popLayout *> ghostRange TcEnd

-- Probably will not throw errors because all the data is determined
-- by the lexer

fromRight :: Either e r -> r
fromRight (Right b) = b
fromRight _ = error "Cannot unpack the data (error on lexer UwU)"

handleEOF :: Lexer (Ranged Token)
handleEOF = do
	layout <- lastLayout
	code <- popCode
	when (code == str) $ do
			pos <- State.gets (currentPos . input)
			Error.throwError $ UnfinishedStr pos
	case layout of
			Nothing -> popCode *> ghostRange TcEOF
			Just _  -> popLayout *> ghostRange TcEnd

scan :: Lexer (Ranged Token)
scan = do
		inputCode <- State.gets input
		code <- startCode
		case alexScan inputCode code of
			AlexEOF              -> handleEOF
			AlexError inp        -> Error.throwError $ UnexpectedChar (currentPos inp)
			AlexSkip restInput _ -> setInput restInput >> scan
			AlexToken input' tokl action -> do
				pos <- State.gets (currentPos . input)
				setInput input'
				action (decodeUtf8 $ ByteString.take tokl inputCode.inputStream) pos
	where
		setInput :: AlexInput -> Lexer ()
		setInput input' = State.modify $ \s -> s { input = input' }

}

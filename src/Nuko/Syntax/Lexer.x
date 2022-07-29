{

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-monomorphism-restriction #-}

module Nuko.Syntax.Lexer where

import Relude hiding (undefined)
import Prelude (undefined)

import Data.Text.Read       (decimal)
import Nuko.Utils           (flag)
import Nuko.Report.Message  (Severity(..))

import Nuko.Syntax.Error
import Nuko.Syntax.Lexer.Support
import Nuko.Syntax.Lexer.Tokens
import Nuko.Report.Range

import qualified Control.Monad.State     as State
import qualified Data.ByteString         as ByteString

}

$newline = [\n\r]
$space   = [\t\v\f\ ]
$digit   = [0-9]
$lower   = [a-z]
$upper   = [A-Z]
$letter  = [a-zA-Z]
$symbol  = [\+\-\*\/\<\>\=\^\?]
$end     = [\?]
$graphic = [\x21-\x7E]

@id_char   = $letter | $digit | _
@lower_id  = $lower @id_char* $end?
@upper_id  = $upper @id_char* $end?
@wild      = _
@number    = $digit+
@not_id    = [^a-zA-Z0-9\ \n\r\t:=\(\)\{\}\:\|\.\,\-\>\\=>]+
@stringraw = [$graphic$space]|$newline

lexer :-

<0> $space+     ;
<0> $newline+   { \_ _ -> pushCode newline *> scan }

<0> "--"        { \_ _ -> pushCode linecom *> scan }

<linecom> [^\n] ;
<linecom> \n    { \_ _ ->  popCode *> scan }
<linecom> eof   { \_ _ ->  popCode *> scan }

<0> "type"      { nonLWToken TcType  }
<0> "with"      { layoutKw TcWith    }
<0> "let"       { token TcLet        }
<0> "as"        { token TcAs         }
<0> "match"     { token TcMatch      }
<0> "import"    { token TcImport     }
<0> "if"        { layoutKw TcIf      }
<0> "then"      { layoutKw TcThen    }
<0> "else"      { layoutKw TcElse    }
<0> "forall"    { token TcForall     }
<0> "pub"       { token TcPub        }
<0> \"          { \_ pos -> do
                     pushCode str
                     res <- scan
                     pure (Ranged (info res) (Range pos (end $ position res)))
                }

<str> \"        { \_ pos  -> popCode *> resetBuffer >>= \s -> emit TcStr s pos  }
<str> .         { \char _ -> addToBuffer char *> scan }

<0> @lower_id   { emit TcLowerId  }
<0> @upper_id   { emit TcUpperId  }

<0> @number     { emit $ TcInt . fst . unsafeRight . decimal }

<0> "="         { layoutKw TcEqual }
<0> "_"         { token TcWild   }
<0> "("         { token TcLPar   }
<0> ")"         { token TcRPar   }
<0> "{"         { token TcLBracket }
<0> "}"         { token TcRBracket }
<0> ":"         { token TcColon  }
<0> "|"         { token TcPipe   }
<0> "."         { token TcDot    }
<0> ","         { token TcComma  }
<0> "->"        { token TcArrow  }
<0> "\"         { token TcSlash  }
<0> "=>"        { layoutKw TcDoubleArrow  }

<0> @not_id     { \_ p -> flagChar (UnexpectedStr) p *> scan }

-- Rule for parsing layout

<layout_> $space+    ;
<layout_> $newline+  ;
<layout_> "--"       { \_ _ -> pushCode linecom *> scan }
<layout_> ()         { startLayout }

<empty_layout> () { emptyLayout }

<newline> $space+   ;
<newline> $newline  ;
<newline> () { offsideRule }

{

-- Some layout thing helpers

unsafeRight :: Either e r -> r
unsafeRight (Right r) = r
unsafeRight (Left _)  = error "Wot!"

emptyLayout _ _ = popCode *> pushCode newline *> ghostRange TcEnd

layoutKw t text pos = do
    isAfterNonLayoutKW <- State.gets nonLw
    if isAfterNonLayoutKW
        then State.modify (\s -> s { nonLw = False })
        else pushCode layout_
    token t text pos

startLayout _ _ = do
    _   <- popCode
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

handleEOF :: Lexer (Ranged Token)
handleEOF = do
    layout <- lastLayout
    code <- popCode
    when (code == str) $ do
        pos <- State.gets (currentPos . input)
        emitDiagnostic Error (UnfinishedStr pos)
    case layout of
        Nothing -> popCode *> ghostRange TcEOF
        Just _  -> popLayout *> ghostRange TcEnd

scan :: Lexer (Ranged Token)
scan = do
        inputCode <- State.gets input
        code <- startCode
        case alexScan inputCode code of
            AlexEOF              -> handleEOF
            AlexError inp        -> do
                emitDiagnostic Error (UnexpectedStr (oneColRange (currentPos inp)))
                case alexGetByte inp of
                    Just (_, inp) -> setInput inp >> scan
                    Nothing -> handleEOF
            AlexSkip restInput _ -> setInput restInput >> scan
            AlexToken input' tokl action -> do
                pos <- State.gets (currentPos . input)
                setInput input'
                action (decodeUtf8 $ ByteString.take tokl inputCode.inputStream) pos
    where
        setInput :: AlexInput -> Lexer ()
        setInput input' = State.modify $ \s -> s { input = input' }
}

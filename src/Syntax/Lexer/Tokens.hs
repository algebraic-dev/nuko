module Syntax.Lexer.Tokens (Token(..)) where 

import Data.Text (Text)

data Token 
    = TknLowerId Text 
    | TknUpperId Text
    | TknSymbol Text
    
    | TknNumber Integer
    | TknLStr Text

    -- Layout 
    | TknOpen | TknClose | TknEnd

     -- Ponctuations
    | TknLPar | TknRPar | TknLBrace | TknRBrace
    | TknEq | TknColon | TknPipe | TknRArrow | TknSlash
    | TknComma | TknDot

    -- Keywords
    | TknKwType | TknKwLet | TknKwDo | TknKwIf 
    | TknKwThen | TknKwElse | TknKwWith | TknKwMatch 

    | TknEOF
    deriving (Show, Eq)
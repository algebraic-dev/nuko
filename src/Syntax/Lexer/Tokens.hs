module Syntax.Lexer.Tokens (Token(..)) where 

import Data.Text (Text)

data Token 
    = TknLowerId Text 
    | TknUpperId Text
    | TknSymbol Text
    
    | TknNumber Integer
    | TknLStr Text
    | TknLChar Char 
    | TknLDouble Double

    -- Layout 
    | TknOpen | TknClose | TknEnd | TknWild

     -- Ponctuations
    | TknLPar | TknRPar | TknLBrace | TknRBrace
    | TknEq | TknColon | TknPipe | TknRArrow | TknSlash
    | TknComma | TknDot | TknDoubleRArrow

    -- Keywords
    | TknKwType | TknKwLet | TknKwDo | TknKwIf 
    | TknKwThen | TknKwElse | TknKwWith | TknKwMatch 
    | TknKwImport | TknKwAs | TknKwExternal

    | TknEOF
    deriving (Show, Eq)
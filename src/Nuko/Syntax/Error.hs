module Nuko.Syntax.Error (
  SyntaxError(..),
  Case(..),
  getErrorSite,
) where

import Relude

import Nuko.Report.Range        (HasPosition (..), Pos, Range, Ranged (..),
                                 oneColRange, toLabel)
import Nuko.Report.Text         (Annotation (..), Color (..), Mode (..),
                                 Piece (..), PrettyDiagnostic (..),
                                 colorlessFromFormat, mkBasicDiagnostic)
import Nuko.Syntax.Lexer.Tokens (Token)

import Data.Aeson               (KeyValue ((.=)), ToJSON (..), object)

data Case = UpperCase | LowerCase
  deriving Show

data SyntaxError
  = UnexpectedStr Range
  | UnfinishedStr Pos
  | UnexpectedToken (Ranged Token)
  | AssignInEndOfBlock Range
  | WrongUsageOfCase Case Range
  deriving Show

errorCode :: SyntaxError -> Int
errorCode = \case
  UnexpectedStr {}      -> 1
  UnfinishedStr {}      -> 2
  UnexpectedToken {}    -> 3
  AssignInEndOfBlock {} -> 4
  WrongUsageOfCase {}   -> 5

getErrorSite :: SyntaxError -> Range
getErrorSite = \case
  UnexpectedStr r      -> r
  UnfinishedStr r      -> oneColRange r
  UnexpectedToken r    -> getPos r
  AssignInEndOfBlock r -> r
  WrongUsageOfCase _ r -> r

errorTitle :: SyntaxError -> Mode
errorTitle = \case
  UnexpectedStr _ -> Words [Raw "Unexpected token"]
  UnfinishedStr _ -> Words [Raw "Unfinished string"]
  UnexpectedToken _ -> Words [Raw "Unexpected token"]
  AssignInEndOfBlock _ -> Words [Raw "You cannot assign a new variable in the end of a block!"]
  WrongUsageOfCase UpperCase _ -> Words [Raw "The identifier should be upper cased"]
  WrongUsageOfCase LowerCase _ -> Words [Raw "The identifier should be lower cased"]

instance PrettyDiagnostic SyntaxError where
  prettyDiagnostic cause =
    let (Words title) = errorTitle cause in
    mkBasicDiagnostic title [Ann Fst (Words [Raw "Here!"]) (getErrorSite cause)]

instance ToJSON SyntaxError where
  toJSON reason =
    object [ "code" .= errorCode reason
           , "place" .= toLabel (getErrorSite reason)
           , "title" .= colorlessFromFormat (errorTitle reason)
           ]

module Error.Message where 

import Data.Text (Text)
import Syntax.Bounds (Pos, Bounds)

import qualified Syntax.Bounds as B

data ErrorKind 
    = UnfinishedString Pos 
    | UnrecognizableChar Pos
    | UnexpectedToken Bounds
    deriving Show

-- Data Components

data ErrComponent
    = Code Bounds (Maybe Text)
    | Desc Text

data ErrMessage = 
    ErrMessage { errBounds     :: Maybe Pos
               , errTitle      :: Text
               , errComponents :: [ErrComponent] }

data ErrReport =
    ErrReport { reportFile :: Text  
              , reportContent :: Text
              , reportKind :: ErrorKind } 

-- Instatiation

onlyCol :: Pos -> Bounds
onlyCol pos@(B.Pos line col) = B.Bounds pos (B.Pos line (col + 1))

messageFromErr :: ErrorKind -> ErrMessage
messageFromErr (UnfinishedString pos) = 
    ErrMessage (Just pos) 
               "Probably you forgot to close a quote while trying to create a string!"  
               [ Code (onlyCol pos) Nothing ]

messageFromErr (UnrecognizableChar pos) = 
    ErrMessage (Just pos) 
               "Cannot understand this character bro UwU"  
               [ Code (onlyCol pos) (Just "Here!") ]

messageFromErr (UnexpectedToken pos) = 
    ErrMessage (Just (B.start pos)) 
               "Cannot uwndustwand this tUwUken"  
               [ Code pos (Just "Not here >:C it's a joke haha") ]

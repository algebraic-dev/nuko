module Syntax.Lexer.Support where 

import Data.Text                (Text)
import Data.List                (uncons)
import Data.List.NonEmpty       (NonEmpty((:|)))
import Data.Word                (Word8)
import Data.ByteString          (ByteString)
import Data.ByteString.Internal (w2c)
import Control.Monad.State      (MonadState)
import Control.Monad.Except     (MonadError)

import qualified Error.Message       as Err
import qualified Syntax.Bounds       as B
import qualified Control.Monad.State as State
import qualified Data.List.NonEmpty  as NonEmpty
import qualified Data.ByteString     as ByteString 

data AlexInput = AlexInput 
    { inputPos     :: B.Pos
    , inputLast    :: Word8
    , inputStream  :: ByteString } deriving Show

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input = update <$> ByteString.uncons (inputStream input)
    where newPos = B.advancePos (inputPos input) . w2c
          update (char, rest) = 
            (char, input { inputPos = newPos char
                         , inputStream = rest
                         , inputLast = char })

alexInputPrevChar :: AlexInput -> Word8
alexInputPrevChar = inputLast

-- Lexer state to store the codes

data LexerState = LexerState 
    { lsInput  :: AlexInput
    , lsCodes  :: NonEmpty Int
    , lsLayout :: [Int]
    , lsBuffer :: Text  }

newtype Lexer a = Lexer { getLexer ::  State.StateT LexerState (Either Err.ErrorKind) a}
    deriving (Functor, Applicative, Monad, MonadState LexerState, MonadError Err.ErrorKind)

initState :: ByteString -> LexerState
initState bs = LexerState (AlexInput (B.Pos 0 1) 10 bs) (0 :| []) [] ""

runLexer :: Lexer a -> ByteString -> Either Err.ErrorKind a 
runLexer lexer bs = fst <$> State.runStateT (getLexer lexer) (initState bs)

-- Some primitives to emitting

startCode :: Lexer Int
startCode = State.gets (NonEmpty.head . lsCodes)

emit :: (Text -> a) -> Text -> B.Pos -> Lexer (B.WithBounds a)
emit fn text pos = do 
    lastPos <- State.gets (inputPos . lsInput)
    pure (B.WithBounds (fn text) (B.Bounds pos lastPos))

token :: a -> Text -> B.Pos -> Lexer (B.WithBounds a)
token = emit . const

pushCode :: Int -> Lexer ()
pushCode code = State.modify (\s -> s { lsCodes = NonEmpty.cons code (lsCodes s)}) 

popCode :: Lexer ()
popCode = State.modify $ \s -> 
    case snd $ NonEmpty.uncons (lsCodes s) of
        Just r  -> s { lsCodes = r }
        Nothing -> s { lsCodes = 0 :| []}

replaceCode :: Int -> Lexer ()
replaceCode code = popCode *> pushCode code

-- Primitives for layout parsing

pushLayout :: Int -> Lexer ()
pushLayout layout = State.modify (\s -> s { lsLayout = layout : lsLayout s }) 

popLayout :: Lexer ()
popLayout = State.modify $ \s -> 
    case lsLayout s of
        _ : xs -> s { lsLayout = xs }
        []     -> s { lsLayout = [] }

lastLayout :: Lexer (Maybe Int) 
lastLayout = State.gets (fmap fst . uncons . lsLayout)
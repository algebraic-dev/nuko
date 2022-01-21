module Syntax.Lexer.Support where 

import Data.Text (Text)
import Data.List (uncons)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.ByteString.Internal (w2c)
import Control.Monad.State (MonadState)
import Control.Monad.Except (MonadError)

import qualified Error.Message as ERR
import qualified Syntax.Bounds as B
import qualified Control.Monad.State as ST
import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString as BS 

data AlexInput = AlexInput { inputPos     :: B.Pos
                           , inputLastPos :: B.Pos 
                           , inputLast    :: Word8
                           , inputStream  :: ByteString } deriving Show

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input = update <$> BS.uncons (inputStream input)
    where newPos = B.advancePos (inputPos input) . w2c
          update (char, rest) = 
            (char, input { inputPos = newPos char
                         , inputStream = rest
                         , inputLast = char })

alexInputPrevChar :: AlexInput -> Word8
alexInputPrevChar = inputLast

-- Lexer state to store the codes

data LexerState = LexerState { lsInput  :: AlexInput
                             , lsCodes  :: NonEmpty Int 
                             , lsLayout :: [Int]
                             , lsBuffer :: Text  }

newtype Lexer a = Lexer { getLexer ::  ST.StateT LexerState (Either ERR.ErrorKind) a}
    deriving (Functor, Applicative, Monad, MonadState LexerState, MonadError ERR.ErrorKind)

initState :: ByteString -> LexerState
initState bs = LexerState (AlexInput (B.Pos 0 1) (B.Pos 0 1) 10 bs) (0 :| []) [] ""

runLexer :: Lexer a -> ByteString -> Either ERR.ErrorKind a 
runLexer lexer bs = fst <$> ST.runStateT (getLexer lexer) (initState bs)

-- Some primitives to emitting

startCode :: Lexer Int
startCode = ST.gets (NE.head . lsCodes)

upPos :: AlexInput -> AlexInput
upPos input' = input' { inputLastPos = inputPos input' }

updateLastPos :: Lexer ()
updateLastPos = ST.modify (\s -> s { lsInput = upPos (lsInput s) })

emit :: (Text -> a) -> Text -> Lexer a 
emit fn text = pure (fn text)

token :: a -> Text -> Lexer a 
token tkn _ = pure tkn

pushCode :: Int -> Lexer ()
pushCode code = ST.modify (\s -> s { lsCodes = NE.cons code (lsCodes s)}) 

popCode :: Lexer ()
popCode = ST.modify $ \s -> 
    case snd $ NE.uncons (lsCodes s) of
        Just r  -> s { lsCodes = r }
        Nothing -> s { lsCodes = 0 :| []}

replaceCode :: Int -> Lexer ()
replaceCode code = popCode *> pushCode code

-- Primitives for layout parsing

pushLayout :: Int -> Lexer ()
pushLayout layout = ST.modify (\s -> s { lsLayout = layout : lsLayout s }) 

popLayout :: Lexer ()
popLayout = ST.modify $ \s -> 
    case lsLayout s of
        _ : xs -> s { lsLayout = xs }
        []     -> s { lsLayout = [] }

lastLayout :: Lexer (Maybe Int) 
lastLayout = ST.gets (fmap fst . uncons . lsLayout)
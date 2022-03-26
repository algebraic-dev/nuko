module Syntax.Lexer.Support where

import Control.Monad.Except (MonadError)
import Control.Monad.State (MonadState)

import Data.ByteString (ByteString)
import Data.ByteString.Internal (w2c)
import Data.List (uncons)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Word (Word8)

import Syntax.Lexer.Tokens (Token (TknEOF))

import qualified Syntax.Bounds as B
import qualified Error.Message as Err
import qualified Control.Monad.State as State
import qualified Data.ByteString as ByteString
import qualified Data.List.NonEmpty as NonEmpty

-- | Definition of errors of the lexer, it uses a type class
-- to generate some error messages. This data type includes
-- errors from parser because the parser uses the same monadT.
data ErrKind
  = UnfinishedString B.Pos
  | UnrecognizableChar B.Pos
  | UnexpectedToken (B.WithBounds Token)
  | UnexpectedAssign B.Bounds

instance Err.ErrorReport ErrKind where
  toErrMessage = \case
    UnfinishedString pos -> Err.columnError pos (Err.normal "Probably you forgot to close a quote while trying to create a string!")
    UnrecognizableChar pos -> Err.columnError pos (Err.normal "Cannot understand this character bro UwU")
    (UnexpectedToken (B.WithBounds TknEOF pos)) -> Err.boundsError pos (Err.normal "Unexpected end of file! ")
    (UnexpectedToken (B.WithBounds _ pos)) -> Err.boundsError pos (Err.normal "Cannot uwndustwand this tUwUken")
    (UnexpectedAssign pos) -> Err.boundsError pos (Err.normal "You cant use let expressions in the end of a block")

-- AlexInput is the Data Type used by the Alex lexer generator
-- to track the position, input and last word/char inside the
-- generated lexer.
data AlexInput = AlexInput
  { inputPos :: B.Pos,
    inputLast :: Word8,
    inputStream :: ByteString
  }
  deriving (Show)

-- "Modifies" the AlexInput and gets the "current" word8/char.
-- It's a required function to the Alex generated lexer.
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input = update <$> ByteString.uncons (inputStream input)
  where
    newPos = B.advancePos (inputPos input) . w2c
    update (char, rest) =
      ( char,
        input
          { inputPos = newPos char,
            inputStream = rest,
            inputLast = char
          }
      )

-- Function required by the Alex to get the previous character.
alexInputPrevChar :: AlexInput -> Word8
alexInputPrevChar = inputLast

-- Lexer state to store the input, each code (the place like (<thing>)
-- that it is in the lexer) and each "layout" place to work by indentation.
data LexerState = LexerState
  { lsInput :: AlexInput,
    lsCodes :: NonEmpty Int,
    lsLayout :: [Int],
    lsBuffer :: Text,
    lsAfterNonLW :: Bool
  }

newtype Lexer a = Lexer {getLexer :: State.StateT LexerState (Either ErrKind) a}
  deriving (Functor, Applicative, Monad, MonadState LexerState, MonadError ErrKind)

initState :: ByteString -> LexerState
initState bs = LexerState (AlexInput (B.Pos 0 1) 10 bs) (0 :| []) [] "" False

runLexer :: Lexer a -> ByteString -> Either ErrKind a
runLexer lexer bs = fst <$> State.runStateT (getLexer lexer) (initState bs)

-- Emition of start code / tokens.

startCode :: Lexer Int
startCode = State.gets (NonEmpty.head . lsCodes)

emit :: (Text -> a) -> Text -> B.Pos -> Lexer (B.WithBounds a)
emit fn text pos = do
  lastPos <- State.gets (inputPos . lsInput)
  pure (B.WithBounds (fn text) (B.Bounds pos lastPos))

token :: a -> Text -> B.Pos -> Lexer (B.WithBounds a)
token = emit . const

pushCode :: Int -> Lexer ()
pushCode code = State.modify (\s -> s {lsCodes = NonEmpty.cons code (lsCodes s)})

popCode :: Lexer ()
popCode = State.modify $ \s ->
  case snd $ NonEmpty.uncons (lsCodes s) of
    Just r -> s {lsCodes = r}
    Nothing -> s {lsCodes = 0 :| []}

replaceCode :: Int -> Lexer ()
replaceCode code = popCode *> pushCode code

-- Layout parsing

pushLayout :: Int -> Lexer ()
pushLayout layout = State.modify (\s -> s {lsLayout = layout : lsLayout s})

popLayout :: Lexer ()
popLayout = State.modify $ \s ->
  case lsLayout s of
    _ : xs -> s {lsLayout = xs}
    [] -> s {lsLayout = []}

lastLayout :: Lexer (Maybe Int)
lastLayout = State.gets (fmap fst . uncons . lsLayout)
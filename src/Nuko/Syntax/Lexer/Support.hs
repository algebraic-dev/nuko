-- | Provides alex types and functions to the lexer.
module Nuko.Syntax.Lexer.Support (
    LexerState(..),
    AlexInput(..),
    Lexer(..),
    alexInputPrevChar,
    alexGetByte,
    emit,
    initialState,
    startCode,
    token,
    pushCode,
    popCode,
    pushLayout,
    popLayout,
    lastLayout
) where

import Control.Monad.Except     (MonadError)
import Control.Monad.State      (MonadState)
import Data.Text                (Text)
import Data.Word                (Word8)
import Data.Maybe               (fromMaybe)
import Data.List                (uncons)
import Data.List.NonEmpty       (NonEmpty ((:|)))
import Data.ByteString          (ByteString)
import Data.ByteString.Internal (w2c)
import Nuko.Syntax.Range        (Point (Point), advancePos, Ranged(..), Range (..))

import qualified Control.Monad.State as State
import qualified Data.ByteString     as ByteString
import qualified Data.List.NonEmpty  as NonEmpty

-- | AlexInput is the Data Type used by the Alex inside the
-- generated code to track the input data.
data AlexInput = AlexInput
    { currentPos  :: Point
    , lastInput   :: Word8
    , inputStream :: ByteString
    }

-- | "Modifies" the AlexInput and gets the "current" word8/char.
-- It's a required function to the Alex generated lexer.
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input = update <$> ByteString.uncons (inputStream input)
  where
    newPos = advancePos (currentPos input) . w2c
    update (char, rest) = (char, (AlexInput (newPos char) char rest))

-- | Function required by the Alex to get the previous character.
alexInputPrevChar :: AlexInput -> Word8
alexInputPrevChar = lastInput

-- | Lexer state to store the input, each code (the place like (<thing>)
-- that it is in the lexer) and each "layout" place to work by indentation.
data LexerState = LexerState
    { input     :: AlexInput
    , codes     :: NonEmpty Int
    , layout    :: [Int]
    , buffer    :: ByteString
    -- It's useful to things like "=" that is a layout keyword only when
    -- it's on a let binding
    , nonLw     :: Bool
    }

initialState :: ByteString -> LexerState
initialState str = -- 10 is the ascii for \n
  LexerState { input    = AlexInput (Point 0 0) 10 str
             , codes    = 0 :| []
             , buffer   = ByteString.empty
             , layout   = []
             , nonLw    = False
             }

newtype Lexer a = Lexer { getLexer :: State.StateT LexerState (Either String) a }
  deriving newtype (Functor, Applicative, Monad, MonadState LexerState, MonadError String)

-- Stack code manipulation things to jump to string, comments and these things.

startCode :: Lexer Int
startCode = State.gets (NonEmpty.head . codes)

pushCode :: Int -> Lexer ()
pushCode code = State.modify $ \s -> s {codes = NonEmpty.cons code s.codes}

popCode :: Lexer Int
popCode = State.state $ \s ->
  let state = s { codes = fromMaybe (0 :| []) (snd $ NonEmpty.uncons s.codes) }
  in (NonEmpty.head s.codes, state)

-- Layout manipulation

pushLayout :: Int -> Lexer ()
pushLayout layout = State.modify $ \s -> s {layout = layout : s.layout}

popLayout :: Lexer ()
popLayout = State.modify $ \s -> s { layout = case s.layout of {_ : xs -> xs; [] -> []} }

lastLayout :: Lexer (Maybe Int)
lastLayout = State.gets (fmap fst . uncons . layout)

-- Token emission things

emit :: (Text -> a) -> Text -> Point -> Lexer (Ranged a)
emit fn text pos = do
  lastPos <- State.gets (currentPos . input)
  pure (Ranged (fn text) (Range pos lastPos))

token :: a -> Text -> Point -> Lexer (Ranged a)
token = emit . const


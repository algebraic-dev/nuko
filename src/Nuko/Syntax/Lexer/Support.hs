-- | Provides alex types and functions to the lexer.
module Nuko.Syntax.Lexer.Support (
    LexerState(..),
    AlexInput(..),
    Lexer(..),
    alexGetByte,
    emit
, alexInputPrevChar, startCode) where

import Data.Word                (Word8)
import Data.List.NonEmpty       (NonEmpty)
import Data.ByteString          (ByteString)
import Data.ByteString.Internal (w2c)
import Control.Monad.Except     (MonadError)
import Control.Monad.State      (MonadState)
import Nuko.Syntax.Range        (Point, advancePos, Ranged(..), Range (..))

import qualified Data.ByteString     as ByteString
import qualified Control.Monad.State as State
import qualified Data.List.NonEmpty  as NonEmpty
import Data.Text (Text)

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

-- Lexer state to store the input, each code (the place like (<thing>)
-- that it is in the lexer) and each "layout" place to work by indentation.
data LexerState = LexerState
    { input  :: AlexInput
    , codes  :: NonEmpty Int
    , layout :: [Int]
    , buffer :: Word8
    , nonLw  :: Bool
    }

newtype Lexer a = Lexer { getLexer :: State.StateT LexerState (Either String) a }
  deriving newtype (Functor, Applicative, Monad, MonadState LexerState, MonadError String)

-- Function required by the Alex to get the previous character.
alexInputPrevChar :: AlexInput -> Word8
alexInputPrevChar = lastInput

startCode :: Lexer Int
startCode = State.gets (NonEmpty.head . codes)

emit :: (Text -> a) -> Text -> Point -> Lexer (Ranged a)
emit fn text pos = do
  lastPos <- State.gets (currentPos . input)
  pure (Ranged (fn text) (Range pos lastPos))
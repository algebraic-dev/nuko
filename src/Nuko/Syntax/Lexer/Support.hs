module Nuko.Syntax.Lexer.Support (
  AlexInput(..),
  LexerState(..),
  Lexer(..),
  terminateWith,
  emitDiagnostic,
  alexGetByte,
  alexInputPrevChar,
  initialState,
  startCode,
  pushCode,
  popCode,
  pushLayout,
  popLayout,
  lastLayout,
  emit,
  token,
  setNonLW,
  nonLWToken,
  resetBuffer,
  addToBuffer,
  ghostRange,
  runLexer,
  flagChar,
) where

import Relude

import Nuko.Names               (ModName)
import Nuko.Report.Message      (Diagnostic (..), DiagnosticInfo (SyntaxError),
                                 Severity (Error))
import Nuko.Report.Range        (Pos, Range, Ranged)
import Nuko.Syntax.Error        (SyntaxError, getErrorSite)
import Nuko.Utils               (flag, terminate)

import Control.Monad.Chronicle  (Chronicle, MonadChronicle)
import Data.ByteString.Internal (w2c)
import Data.These               (These)

import Control.Monad.Chronicle  qualified as Chronicle
import Control.Monad.State      qualified as State
import Data.ByteString          qualified as ByteString
import Data.List.NonEmpty       qualified as NonEmpty
import Nuko.Report.Range        qualified as Range

-- | AlexInput is the Data Type used by the Alex inside the
-- generated code to track the input data.
data AlexInput = AlexInput
    { currentPos  :: Pos
    , lastInput   :: Word8
    , inputStream :: ByteString
    }

-- | "Modifies" the AlexInput and gets the "current" word8/char.
-- It's a required function to the Alex generated lexer.
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input' = update <$> ByteString.uncons (inputStream input')
  where newPos = Range.advancePos (currentPos input')  . w2c
        update (char, rest) = (char, AlexInput (newPos char) char rest)

-- | Function required by the Alex to get the previous character.
alexInputPrevChar :: AlexInput -> Word8
alexInputPrevChar = lastInput

-- | Lexer state to store the input, each code (the place like (<thing>)
-- that it is in the lexer) and each "layout" place to work by indentation.
data LexerState = LexerState
    { modName  :: ModName
    , filename :: Text
    , input    :: AlexInput
    , codes    :: NonEmpty Int
    , layout   :: [Int]
    , buffer   :: Text
    , nonLw    :: Bool
    }

initialState :: ModName -> Text -> ByteString -> LexerState
initialState moduleName' fileName str = -- 10 is the ascii for \n
  LexerState { modName  = moduleName'
             , filename = fileName
             , input    = AlexInput (Range.Pos 0 0) 10 str
             , codes    = 0 :| []
             , buffer   = ""
             , layout   = []
             , nonLw    = False
             }

newtype Lexer a = Lexer { getLexer :: State.StateT LexerState (Chronicle (Endo [Diagnostic])) a }
  deriving newtype (Functor, Applicative, Monad, MonadState LexerState, MonadChronicle (Endo [Diagnostic]))

-- Stack code manipulation things to jump to string, comments and these things.

startCode :: Lexer Int
startCode = State.gets (NonEmpty.head . codes)

pushCode :: Int -> Lexer ()
pushCode code = State.modify $ \s -> s {codes = NonEmpty.cons code s.codes}

popCode :: Lexer Int
popCode = State.state $ \s ->
  let state' = s { codes = fromMaybe (0 :| []) (snd $ NonEmpty.uncons s.codes) }
  in (head s.codes, state')

-- Layout manipulation

pushLayout :: Int -> Lexer ()
pushLayout layout' = State.modify $ \s -> s {layout = layout' : s.layout}

popLayout :: Lexer ()
popLayout = State.modify $ \s -> s { layout = case s.layout of {_ : xs -> xs; [] -> []} }

lastLayout :: Lexer (Maybe Int)
lastLayout = State.gets (fmap fst . uncons . layout)

-- Error messages

mkCompilerError :: Severity -> SyntaxError -> Lexer Diagnostic
mkCompilerError diagSeverity err = do
  name <- gets modName
  fileName <- gets Nuko.Syntax.Lexer.Support.filename
  pure $ Diagnostic
    { moduleName = name
    , filename   = fileName
    , severity   = diagSeverity
    , position   = getErrorSite err
    , kind       = SyntaxError err
    }

flagChar :: (Range -> SyntaxError) -> Pos -> Lexer ()
flagChar fn pos = do
  lastPos <- State.gets (currentPos . input)
  flag =<< mkCompilerError Error (fn (Range.Range pos lastPos))

emitDiagnostic :: Severity -> SyntaxError -> Lexer ()
emitDiagnostic diagnosticSeverity err = flag =<< mkCompilerError diagnosticSeverity err

terminateWith :: SyntaxError -> Lexer a
terminateWith err = terminate =<< mkCompilerError Error err

-- Token emission things

emit :: (Text -> a) -> Text -> Pos -> Lexer (Ranged a)
emit fn text pos = do
  lastPos <- State.gets (currentPos . input)
  pure (Range.Ranged (fn text) (Range.Range pos lastPos))

token :: a -> Text -> Pos -> Lexer (Ranged a)
token = emit . const

setNonLW :: Lexer ()
setNonLW = State.modify (\s -> s { nonLw = True })

nonLWToken :: a -> Text -> Pos -> Lexer (Ranged a)
nonLWToken a t p = setNonLW >> emit (const a) t p

-- Buffer manipulation

resetBuffer :: Lexer Text
resetBuffer = State.state (\s -> (s.buffer, s { buffer = "" }))

addToBuffer :: Text -> Lexer ()
addToBuffer text = State.modify (\s -> s { buffer = s.buffer <> text })

ghostRange :: t -> Lexer (Ranged t)
ghostRange t = do
    pos <- State.gets (currentPos . input)
    pure $ Range.Ranged t (Range.Range pos pos)

runLexer :: Lexer a -> ModName -> Text -> ByteString -> These [Diagnostic] a
runLexer lex' moduleName' fileName input' =
  first
    (`appEndo` [])
    (Chronicle.runChronicle
      $ State.evalStateT lex'.getLexer (initialState moduleName' fileName input'))

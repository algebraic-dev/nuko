module Nuko.Typer.Error
  ( TypeError (..),
    ErrCause(..),
    typeError,
    Extractor
  )
where

import Nuko.Error.Data
import Nuko.Syntax.Range           (Range)
import Control.Exception      (Exception)
import Data.Text              (Text)
import Nuko.Typer.Env         (Name, Env (trackers), TyperMonad, Tracker (..))
import Nuko.Typer.Types       (Ty)
import Control.Exception.Base (throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Sequence          (Seq((:|>)))
import Nuko.Syntax.Range      (getPos, Range(..))
import Data.Maybe             (fromJust, isJust)
import Control.Applicative    (empty)

import qualified Control.Monad.State       as State
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Control.Monad.Reader      as Reader
import qualified Data.Sequence             as Seq
import qualified Data.Foldable             as Foldable
import qualified Data.Text as Text

data ErrCause
  = CannotFind Range Name
  | CannotFindType Range Text
  | CannotFindModule Range Text
  | CannotUnify
  | OccoursCheck Ty Ty
  | EscapingScope Ty
  | NotAFunction Ty
  deriving Show

data TypeError = TypeError { cause :: ErrCause, environment :: Env }

instance Show TypeError where
  show err = show err.cause

instance Exception TypeError

typeError :: (TyperMonad m) => ErrCause -> m b
typeError cause' = do
  env <- State.get
  liftIO $ throwIO $ TypeError cause' env

-- Extracting error messages from context here.

type Extractor e a = Maybe.MaybeT (Reader.Reader e) a

runExtractor :: Extractor TypeError a -> TypeError -> Maybe a
runExtractor ext ty = Reader.runReader (Maybe.runMaybeT ext) ty

-- | Gets the position of the last term found in the trackers
--   tree.
lastTerm :: Extractor TypeError Range
lastTerm =
    Reader.asks (trackers . environment) >>= go
  where go :: Seq Tracker -> Extractor a Range
        go (Seq.Empty) = empty
        go (tail' :|> h) =
          case h of
            InInferExpr expr -> pure (getPos expr)
            InInferTy ty -> pure (getPos ty)
            InInferLit lit -> pure (getPos lit)
            InInferPat pat -> pure (getPos pat)
            InCheckPat pat -> pure (getPos pat)
            InInferGen pos -> pure pos
            InCheck expr _ -> pure (getPos expr)
            InApply _ expr -> pure (getPos expr)
            _ -> go tail'

takeUnpack :: (a -> Maybe b) -> Seq a -> Seq b
takeUnpack fn = fmap (fromJust . fn) . Seq.filter (isJust . fn)

takeSub :: Seq Tracker -> [(Ty, Ty)]
takeSub = Foldable.toList
        . takeUnpack (\case
            (InUnify a b) -> Just (a, b)
            _ -> Nothing)

mismatch :: Report -> Extractor TypeError Report
mismatch report = do
    trackers' <- Reader.asks (trackers . environment)
    range' <- lastTerm
    ((_, _), (gotLeaf, expectedLeaf)) <- firstLast (takeSub trackers')
    pure $ report
        { position = range'.start
        , title = [Normal "Mismatch between types."]
        , markers = 
            [ Ann (getPos gotLeaf) Main (Just $ "The type of this expression is '" <> Text.pack (show $ getPos gotLeaf) <> "'")
            , Ann (getPos expectedLeaf) Main (Just $ "Expected type '" <> Text.pack (show $ getPos expectedLeaf) <> "'")]}
  where
    firstLast :: Monad i => [a] -> Maybe.MaybeT i (a, a)
    firstLast [] = empty
    firstLast tl = pure (head tl, last tl)

instance CompilerError TypeError where
  getReport report err = case err.cause of
    CannotUnify ->
      maybe (error $ "1. Cannot produce an error message:" ++ (show err.cause)) id (runExtractor (mismatch report) err)
    NotAFunction _ ->
      maybe (error $ "2. Cannot produce an error message:" ++ (show err.cause)) id (runExtractor (mismatch report) err)
    CannotFind range' name ->
      report { position = range'.start
             , markers = [ Ann range' Main (Just "Here!") ]
             , title   = [ Normal "Cannot find the variable"
                         , Marked Main name] }
    CannotFindType range' name ->
      report { position = range'.start
             , markers = [ Ann range' Main (Just "Here!") ]
             , title   = [ Normal "Cannot find the type"
                         , Marked Main name] }
    CannotFindModule range' name ->
      report { position = range'.start
             , markers = [ Ann range' Main (Just "Here!") ]
             , title   = [ Normal "Cannot find the module"
                         , Marked Main name] }
    _ -> error $ "3. Cannot produce an error message:" ++ (show err.cause)
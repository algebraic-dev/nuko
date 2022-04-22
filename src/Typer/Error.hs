module Typer.Error
  ( TypeError (..),
    ErrCause(..),
    typeError,
    Extractor
  )
where

import Error.Message
import Syntax.Range           (Range)
import Control.Exception      (Exception)
import Data.Text              (Text)
import Typer.Env              (Name, Env (trackers), TyperMonad, Tracker (..), getTyPos)
import Typer.Types            (Ty)
import Control.Exception.Base (throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Sequence          (Seq((:|>)))
import Syntax.Range           (getPos, Range(..))
import Data.Maybe             (catMaybes, fromJust, isJust)
import Control.Applicative    (empty, Alternative ((<|>)))

import qualified Error.Message             as Err
import qualified Control.Monad.State       as State
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Control.Monad.Reader      as Reader
import qualified Data.Sequence             as Seq
import qualified Data.Foldable             as Foldable

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

mismatch :: Extractor TypeError Err.ErrMessage
mismatch = do
    trackers' <- Reader.asks (trackers . environment)
    range' <- lastTerm
    ((gotBase, expectedBase), (gotLeaf, expectedLeaf)) <- firstLast (takeSub trackers')
    let expectedPosition = getTyPos expectedLeaf <|> getTyPos expectedBase
    let expectedTy = maybe expectedLeaf (const expectedBase) expectedPosition
    pure $
      ErrMessage (Just range'.start)
        [ Normal "Expected type "
        , Colored Red (format expectedLeaf)
        , Normal " but instead got "
        , Colored Blue (format gotLeaf)]
        $ catMaybes
            [ (\ra -> Code Red ra (Just $ "The type of this is '" <> format expectedTy <> "'")) <$> expectedPosition
            , Just $ Code Blue range' (Just $ "The type of this is '" <> format gotBase <> "'")]
  where
    firstLast :: Monad i => [a] -> Maybe.MaybeT i (a, a)
    firstLast [] = empty
    firstLast tl = pure (head tl, last tl)

instance Err.ErrorReport TypeError where
  toErrMessage err = case err.cause of
    CannotUnify ->
      maybe (error $ "1. Cannot produce an error message:" ++ (show err.cause)) id (runExtractor mismatch err)

    NotAFunction _ ->
      maybe (error $ "2. Cannot produce an error message:" ++ (show err.cause)) id (runExtractor mismatch err)

    CannotFind range' name ->
      ErrMessage (Just range'.start)
        [ Normal "Cannot find the variable "
        , Colored Red name]
        [ Code Red range' (Just $ "Cannot find that!")]

    CannotFindType range' name ->
      ErrMessage (Just range'.start)
        [ Normal "Cannot find the type "
        , Colored Red name]
        [ Code Red range' (Just $ "Cannot find this type make sure it's on scope!")]

    CannotFindModule range' name ->
      ErrMessage (Just range'.start)
        [ Normal "Cannot find the module "
        , Colored Red name]
        [ Code Red range' (Just $ "Cannot find this module make sure it's on scope!")]

    _ -> error $ "Cannot produce an error message:" ++ (show err.cause)
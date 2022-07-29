module Nuko.Typer.Error.Extract (
  Tracker(..),
  extractTypeMismatch,
  extractKindMismatch,
  extractTypeUnifierRange,
  extractKindUnifierRange,
  track
) where

import Nuko.Typer.Error.Tracking (Tracker(..), getLastTypeUnify, getLastKindUnify )
import Nuko.Typer.Env            (MonadTyper, teTrackers, emitDiagnostic)
import Nuko.Typer.Error          (TypeError(..))
import Nuko.Report.Text          (Severity(Error))
import Nuko.Report.Range         (Range(..))
import Lens.Micro.Platform       (use, (%=))
import Data.Maybe                (fromMaybe)
import Relude                    (error, Applicative (..))
import Relude.Unsafe             (tail)

track :: MonadTyper m => Tracker -> m a -> m a
track tracker action = (teTrackers %= (tracker :)) *> action <* (teTrackers %= tail)

internalMismatch :: a
internalMismatch = error "Compiler error: A tracker for unification must exist"

extractTypeMismatch :: MonadTyper m => m ()
extractTypeMismatch = do
  trackers <- use teTrackers
  let (r, t, t') = fromMaybe internalMismatch (getLastTypeUnify trackers)
  emitDiagnostic Error (Mismatch r t t') r

extractKindMismatch :: MonadTyper m => m ()
extractKindMismatch = do
  trackers <- use teTrackers
  let (r, t, t') = fromMaybe internalMismatch (getLastKindUnify trackers)
  emitDiagnostic Error (KindMismatch r t t') r

extractKindUnifierRange :: MonadTyper m => m Range
extractKindUnifierRange = do
  trackers <- use teTrackers
  let (r, _, _) = fromMaybe internalMismatch (getLastKindUnify trackers)
  pure r

extractTypeUnifierRange :: MonadTyper m => m Range
extractTypeUnifierRange = do
  trackers <- use teTrackers
  let (r, _, _) = fromMaybe internalMismatch (getLastTypeUnify trackers)
  pure r
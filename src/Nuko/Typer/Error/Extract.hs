module Nuko.Typer.Error.Extract (
  Tracker(..),
  extractTypeMismatch,
  extractKindMismatch,
  extractTypeUnifierRange,
  extractKindUnifierRange,
  track
) where

import Relude                    hiding (tail)
import Relude.Unsafe             (tail)

import Nuko.Report.Range         (Range (..))
import Nuko.Typer.Env            (MonadTyper, endDiagnostic, teTrackers)
import Nuko.Typer.Error          (TypeError (..))
import Nuko.Typer.Error.Tracking (Tracker (..), getLastKindUnify,
                                  getLastTypeUnify)

import Lens.Micro.Platform       (use, (%=))

track :: MonadTyper m => Tracker -> m a -> m a
track tracker action = (teTrackers %= (tracker :)) *> action <* (teTrackers %= tail)

internalMismatch :: a
internalMismatch = error "Compiler error: A tracker for unification must exist"

extractTypeMismatch :: MonadTyper m => m ()
extractTypeMismatch = do
  trackers <- use teTrackers
  let (r, t, t') = fromMaybe internalMismatch (getLastTypeUnify trackers)
  endDiagnostic (Mismatch r t t') r

extractKindMismatch :: MonadTyper m => m ()
extractKindMismatch = do
  trackers <- use teTrackers
  let (r, t, t') = fromMaybe internalMismatch (getLastKindUnify trackers)
  endDiagnostic (KindMismatch r t t') r

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

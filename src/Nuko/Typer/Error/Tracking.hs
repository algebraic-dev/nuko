module Nuko.Typer.Error.Tracking (
  Tracker(..),
  getLastTypeUnify,
  getLastKindUnify
) where

import Nuko.Typer.Types
import Nuko.Report.Range (Range(..))
import Data.Maybe (Maybe (..))
import Relude (Generic, (<>))
import Pretty.Tree (PrettyTree)
import Pretty.Format (Format(format))

data Tracker
  = InUnify Range (TTy 'Virtual) (TTy 'Virtual)
  | InKindUnify Range TKind TKind
  | InApp Range
  deriving Generic

instance Format Tracker where
  format = \case
    InUnify _ t t' -> "Unifying: " <> format t <> ", " <> format t'
    InKindUnify _ t t' -> "Unifying Kind: " <> format t <> ", " <> format t'
    _ -> "Err"

instance PrettyTree Tracker where

getLastTypeUnify :: [Tracker] -> Maybe (Range, TTy 'Virtual, TTy 'Virtual)
getLastTypeUnify (InUnify {} : g@InUnify {} : xs) = getLastTypeUnify (g : xs)
getLastTypeUnify ((InUnify r t t') : _) = Just (r, t, t')
getLastTypeUnify (_ : xs) = getLastTypeUnify xs
getLastTypeUnify [] = Nothing

getLastKindUnify :: [Tracker] -> Maybe (Range, TKind, TKind)
getLastKindUnify (InKindUnify {} : g@InKindUnify {} : xs) = getLastKindUnify (g : xs)
getLastKindUnify ((InKindUnify r t t') : _) = Just (r, t, t')
getLastKindUnify (_ : xs) = getLastKindUnify xs
getLastKindUnify [] = Nothing
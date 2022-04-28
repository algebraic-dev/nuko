-- | This module stores everything related to source code
-- localization. It is only used in the front end while
-- the ast reflects the actual code and in the error module.
module Nuko.Syntax.Range (
    Point(..),
    Range(..),
    Ranged(..),
    advancePos
) where

import qualified Data.List.NonEmpty as NE

data Point = Point {line, column :: Int}

data Range = Range {start, end :: Point}

data Ranged a = Ranged {info :: a, position :: Range}

instance Semigroup Range where
  (Range s _) <> (Range _ e) = Range s e

instance Show Point where
  show (Point line col) = show line ++ ":" ++ show col

instance Show Range where
  show (Range start end) = show start ++ "-" ++ show end

advancePos :: Point -> Char -> Point
advancePos pos '\n' = Point { line = line pos + 1, column = 1}
advancePos pos _    = pos   { column = column pos + 1}

-- Useful type class :D

class HasPosition a where
  getPos :: a -> Range

instance (HasPosition a, HasPosition b) => HasPosition (a, b) where
  getPos (a, b) = getPos a <> getPos b

instance HasPosition a => HasPosition (NE.NonEmpty a) where
  getPos x  = getPos (NE.head x) <> getPos (NE.last x)
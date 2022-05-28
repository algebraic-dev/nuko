-- | This module stores everything related to source code
-- localization. It is only used in the front end while
-- the ast reflects the actual code and in the error module.
module Nuko.Syntax.Range (
    Pos(..),
    Range(..),
    Ranged(..),
    HasPosition(..),
    advancePos,
    oneColRange
) where

import qualified Data.List.NonEmpty as NE

data Pos = Pos {line, column :: Int}

data Range = Range {start, end :: Pos}

data Ranged a = Ranged {info :: a, position :: Range}

instance Show a => Show (Ranged a) where
  show (Ranged info pos) = show (info, pos)

instance Semigroup Range where
  (Range s _) <> (Range _ e) = Range s e

instance Show Pos where
  show (Pos line col) = show (line + 1) ++ ":" ++ show col

instance Show Range where
  show (Range start end) = show start ++ "-" ++ show end

advancePos :: Pos -> Char -> Pos
advancePos pos '\n' = Pos { line = line pos + 1, column = 0}
advancePos pos _    = pos   { column = column pos + 1}

oneColRange :: Pos -> Range
oneColRange point =
  Range
    point
    (point { column = point.column + 1})

-- Useful type class :D

class HasPosition a where
  getPos :: a -> Range

instance (HasPosition a, HasPosition b) => HasPosition (a, b) where
  getPos (a, b) = getPos a <> getPos b

instance HasPosition a => HasPosition (NE.NonEmpty a) where
  getPos x  = getPos (NE.head x) <> getPos (NE.last x)

instance HasPosition a => HasPosition [a] where
  getPos x@(_:_) = getPos (head x) <> getPos (last x)
  getPos [] = error "oh no!"

instance HasPosition (Ranged a) where
    getPos (Ranged _ a) = a

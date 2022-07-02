module Nuko.Syntax.Range (
    Pos(..),
    Range(..),
    Ranged(..),
    HasPosition(..),
    advancePos,
    oneColRange
) where

import Relude (Int, Semigroup(..), Char, Num ((+)), Show)

import qualified Data.List.NonEmpty as NonEmpty

data Pos = Pos { line, column :: Int } deriving Show

data Range = Range { start, end :: Pos } deriving Show

data Ranged a = Ranged { info :: a, position :: Range } deriving Show

instance Semigroup Range where
  (Range s _) <> (Range _ e) = Range s e

advancePos :: Pos -> Char -> Pos
advancePos pos '\n' = Pos { line = pos.line + 1, column = 0 }
advancePos pos _    = pos { column = pos.column + 1 }

oneColRange :: Pos -> Range
oneColRange point =
  Range point
        (point { column = point.column + 1})

-- Useful type class :D

class HasPosition a where
  getPos :: a -> Range

instance (HasPosition a, HasPosition b) => HasPosition (a, b) where
  getPos (a, b) = getPos a <> getPos b

instance HasPosition a => HasPosition (NonEmpty.NonEmpty a) where
  getPos x  = getPos (NonEmpty.head x) <> getPos (NonEmpty.last x)

instance HasPosition (Ranged a) where
    getPos (Ranged _ a) = a
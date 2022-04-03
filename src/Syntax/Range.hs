-- | This module is useful for tracking source code position inside
--     Data structures like Tokens or the Abstract syntax tree. After the
--     Type checking and lowering it is unused
module Syntax.Range
  ( Point (..),
    Range (..),
    Ranged (..),
    Loc (..),
    HasPosition (..),
    advancePos,
  )
where

import Data.Function (on)

data Point = Point
  { line :: !Int,
    column :: !Int
  }
  deriving (Show)

data Range = Range
  { start :: !Point,
    end :: !Point
  } deriving Show

data Ranged a = Ranged
  { info :: a,
    position :: !Range
  }

data Loc a 
  = Blank
  | Loc (Ranged a)

instance Semigroup Range where
  (Range s _) <> (Range _ e) = Range s e

instance Monoid Range where
  mempty = Range (Point 0 0) (Point 0 0)

instance Eq a => Eq (Ranged a) where (==) = (==) `on` info

instance Show a => Show (Ranged a) where
  show a = show $ info a

advancePos :: Point -> Char -> Point
advancePos pos '\n' = Point {line = line pos + 1, column = 1}
advancePos pos _ = pos {column = column pos + 1}

-- Has position interface

class HasPosition a where
  getPos :: a -> Range

instance (HasPosition a, HasPosition b) => HasPosition (a, b) where
  getPos (a, b) = getPos a <> getPos b

instance HasPosition a => HasPosition [a] where
  getPos [] = mempty
  getPos x = getPos (head x) <> getPos (last x)
{-| This module is useful for tracking source code position inside
    Data structures like Tokens or the Abstract syntax tree. After the 
    Type checking and lowering it is unused
-}
module Syntax.Bounds (
  Pos(..),
  Bounds(..),
  WithBounds(..),
  advancePos,
  empty
  ) where

import Data.Function (on)

data Pos = Pos {line :: !Int, column :: !Int} deriving Show
data Bounds = Bounds {start :: !Pos, end :: !Pos}
data WithBounds a = WithBounds {info :: a, position :: !Bounds}

instance Semigroup Bounds where
  (Bounds s _) <> (Bounds _ e) = Bounds s e

instance Eq a => Eq (WithBounds a) where
  (==) = (==) `on` info

instance Show Bounds where 
  show _ = ""


instance Show a => Show (WithBounds a) where 
  show a = show $ info a

advancePos :: Pos -> Char -> Pos
advancePos pos '\n' = Pos {line = line pos + 1, column = 1}
advancePos pos _ = pos {column = column pos + 1}

empty :: Bounds
empty = Bounds (Pos 0 0) (Pos 0 0)
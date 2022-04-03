{-| This module is useful for tracking source code position inside
    Data structures like Tokens or the Abstract syntax tree. After the 
    Type checking and lowering it is unused
-}
module Syntax.Range (
  Pos(..),
  Range(..),
  Ranged(..),
  advancePos,
  empty,
  HasPosition(..)
  ) where

import Data.Function (on)

data Pos = Pos {line :: !Int, column :: !Int} deriving Show
data Range = Range {start :: !Pos, end :: !Pos}
data Ranged a = Ranged {info :: a, position :: !Range}

instance Semigroup Range where
  (Range s _) <> (Range _ e) = Range s e

instance Eq a => Eq (Ranged a) where
  (==) = (==) `on` info

instance Show Range where 
  show _ = ""

instance Show a => Show (Ranged a) where 
  show a = show $ info a

advancePos :: Pos -> Char -> Pos
advancePos pos '\n' = Pos {line = line pos + 1, column = 1}
advancePos pos _ = pos {column = column pos + 1}

empty :: Range
empty = Range (Pos 0 0) (Pos 0 0)

class HasPosition a where 
    getPos :: a -> Range
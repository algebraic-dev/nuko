module Syntax.Bounds where

import Data.Function (on)

data Pos = Pos { line :: !Int, column :: !Int } 
           deriving Show

data Bounds = Bounds { start :: !Pos, end :: !Pos } deriving Show
        
data WithBounds a = WithBounds { info :: a, position :: !Bounds } 
                    deriving Show


instance Semigroup Bounds where 
    (Bounds s _) <> (Bounds _ e) = Bounds s e

instance Eq a => Eq (WithBounds a) where 
    (==) = (==) `on` info
    
advancePos :: Pos -> Char -> Pos 
advancePos pos '\n' = Pos { line = line pos + 1, column = 1 }
advancePos pos _    = pos { column = column pos + 1 }

empty :: Bounds 
empty = Bounds (Pos 0 0) (Pos 0 0) 
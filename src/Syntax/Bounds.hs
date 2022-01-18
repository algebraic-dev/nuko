module Syntax.Bounds (
    Pos(..), 
    Bounds(..), 
    WithBounds(..),
    advancePos,
    emptyBound,
    mixBounds
) where

data Pos = Pos { line :: !Int, column :: !Int } deriving Show
data Bounds = Bounds { start :: !Pos, end :: !Pos }
data WithBounds a = WithBounds { info :: a, bounds :: !Bounds }

instance Show a => Show (WithBounds a) where 
    show res = show (info res)

instance Show Bounds where 
    show r = "_"

advancePos :: Pos -> Char -> Pos 
advancePos pos '\n' = Pos { line = line pos + 1, column = 1 }
advancePos pos c    = pos { column = column pos + 1 }

emptyBound :: Bounds 
emptyBound = Bounds (Pos 0 0) (Pos 0 0) 

mixBounds :: Bounds -> Bounds -> Bounds
mixBounds (Bounds s _) (Bounds _ e) = Bounds s e
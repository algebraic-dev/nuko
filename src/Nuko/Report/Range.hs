module Nuko.Report.Range (
    HasPosition(..),
    SetPosition(..),
    Ranged(..),
    Range(..),
    Pos(..),
    oneColRange,
    advancePos,
    toLabel,
    emptyRange,
    copyPos
) where
import Relude

import Data.List.NonEmpty qualified as NonEmpty
import Pretty.Tree        (PrettyTree (..), Tree (..), inlineTree)

data Pos = Pos
  { line   :: {-# UNPACK #-} Int
  , column :: {-# UNPACK #-} Int
  } deriving Show

data Range = Range
  { start :: Pos
  , end   :: Pos
  } deriving Show

data Ranged a = Ranged
  { info     :: a
  , position :: Range }
  deriving Show

instance Semigroup Range where
  (Range s _) <> (Range _ e) = Range s e

advancePos :: Pos -> Char -> Pos
advancePos pos '\n' = Pos { line = pos.line + 1, column = 0 }
advancePos pos _    = pos { column = pos.column + 1 }

oneColRange :: Pos -> Range
oneColRange point = Range point (point { column = point.column + 1})

emptyRange :: Range
emptyRange = Range (Pos 0 0) (Pos 0 0)

toLabel :: Range -> Text
toLabel r = show r.start.line <> ":" <> show r.start.column <> "-" <> show r.end.line <> ":" <> show r.end.column

copyPos :: (SetPosition a, HasPosition b) => b -> a -> a
copyPos name = setPos (getPos name)

class SetPosition a where
  setPos :: Range -> a -> a

class HasPosition a where
  getPos :: a -> Range

instance (HasPosition a, HasPosition b) => HasPosition (a, b) where
  getPos (a, b) = getPos a <> getPos b

instance HasPosition a => HasPosition (NonEmpty.NonEmpty a) where
  getPos x  = getPos (NonEmpty.head x) <> getPos (NonEmpty.last x)

instance HasPosition (Ranged a) where
    getPos (Ranged _ a) = a

instance PrettyTree Range where
  prettyTree r = Node (toLabel r) [] []

instance PrettyTree a => PrettyTree (Ranged a) where
  prettyTree r =
    let (Node n m e) = inlineTree $ Node "Ranged:" [] [prettyTree r.info] in
    Node n (toLabel r.position : m) e

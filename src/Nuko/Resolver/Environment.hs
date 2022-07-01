module Nuko.Resolver.Environment (
  NameSort(..),
  Visibility(..),
  NameSpace(..),
  Label(..),
  LocalNS(..),
  joinLabels,
  localNames,
  openedNames,
) where

import Nuko.Resolver.Occourence (OccEnv)
import Relude.String            (Text)
import Relude.Container         (HashSet)
import Relude.Base              (Eq((==)))
import Relude.Bool              (otherwise, Bool)
import Relude.Monoid            ((<>))

import qualified Data.HashSet as HashSet
import Lens.Micro.Platform (makeLenses)

data NameSort
  = External Text
  | Internal

data Visibility
  = Public
  | Private

data NameSpace = NameSpace
  { modName :: Text
  , names   :: OccEnv Visibility
  }

-- | It's a collection of module names that are the result of
-- opening multiple things with same name inside an enviroment

data Label = Single Text | Ambiguous (HashSet Text)

joinLabels :: Label -> Label -> Label
joinLabels a' b' = case (a', b') of
  (Single a, Ambiguous b) -> Ambiguous (HashSet.insert a b)
  (Ambiguous b, Single a) -> Ambiguous (HashSet.insert a b)
  (Ambiguous a, Ambiguous b) -> Ambiguous (a <> b)
  (Single a, Single b)
    | a == b    -> Single a
    | otherwise -> Ambiguous (HashSet.fromList [a,b])

-- | This structure stores a lot of occourence names with a label
-- so it can describe if some name is ambiguous or not.

data LocalNS = LocalNS
  { _openedNames :: OccEnv Label
  , _localNames :: OccEnv Bool
  }

makeLenses ''LocalNS


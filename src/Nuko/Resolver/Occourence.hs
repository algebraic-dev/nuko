module Nuko.Resolver.Occourence (
  OccEnv(..),
  getMap,
  lookupOcc,
  insertOcc,
  insertWith,
  lookupAlts,
  empty,
  member,
) where

import Relude              (HashMap, Semigroup, Monoid, Functor (fmap), asum, NonEmpty, Bool)
import Relude.Monad        (Maybe)

import Pretty.Tree         (PrettyTree(..))
import Nuko.Names          (Label(..), NameSort (..), mkName, Ident, Attribute (Untouched))
import Lens.Micro.Platform (makeLenses)


import qualified Data.HashMap.Strict as HashMap

newtype OccEnv a = OccEnv { _getMap :: (HashMap Label a) }
  deriving newtype (Semigroup, Monoid, Functor)

makeLenses ''OccEnv

instance PrettyTree a => PrettyTree (OccEnv a) where
  prettyTree (OccEnv a) = prettyTree a

empty :: OccEnv a
empty = OccEnv HashMap.empty

lookupOcc :: Label -> OccEnv a -> Maybe a
lookupOcc name (OccEnv map') = HashMap.lookup name map'

member :: Label -> OccEnv a -> Bool
member name (OccEnv map') = HashMap.member name map'

insertOcc :: Label -> a -> OccEnv a -> OccEnv a
insertOcc name val (OccEnv map') = OccEnv (HashMap.insert name val map')

insertWith :: Label -> (a -> a -> a) -> a -> OccEnv a -> OccEnv a
insertWith name update value (OccEnv env) = OccEnv (HashMap.insertWith update name value env)

lookupAlts :: NonEmpty NameSort -> Ident -> OccEnv a -> Maybe (NameSort, a)
lookupAlts names ident env = asum (fmap (\sort@(NameSort kind) -> fmap (sort, ) (lookupOcc (Label (mkName kind ident Untouched)) env)) names)
module Nuko.Resolver.Occourence (
  OccEnv(..),
  getMap,
  lookupOcc,
  insertOcc,
  insertWith,
  lookupAlts,
  emptyOcc,
  member,
) where

import Relude

import Nuko.Names          (Attribute (Untouched), Ident, Label (..),
                            NameSort (..), mkName)
import Pretty.Tree         (PrettyTree (..))

import Lens.Micro.Platform (makeLenses)

import Data.HashMap.Strict qualified as HashMap

newtype OccEnv a = OccEnv { _getMap :: HashMap Label a }
  deriving newtype (Semigroup, Monoid, Functor)

makeLenses ''OccEnv

instance PrettyTree a => PrettyTree (OccEnv a) where
  prettyTree (OccEnv a) = prettyTree a

emptyOcc :: OccEnv a
emptyOcc = OccEnv HashMap.empty

lookupOcc :: Label -> OccEnv a -> Maybe a
lookupOcc name (OccEnv map') = HashMap.lookup name map'

member :: Label -> OccEnv a -> Bool
member name (OccEnv map') = HashMap.member name map'

insertOcc :: Label -> a -> OccEnv a -> OccEnv a
insertOcc name val (OccEnv map') = OccEnv (HashMap.insert name val map')

insertWith :: Label -> (a -> a -> a) -> a -> OccEnv a -> OccEnv a
insertWith name update value (OccEnv env) = OccEnv (HashMap.insertWith update name value env)

lookupAlts :: NonEmpty NameSort -> Ident -> OccEnv a -> Maybe (NameSort, a)
lookupAlts names ident env = asum (fmap (\nameSort@(NameSort kind) -> fmap (nameSort, ) (lookupOcc (Label (mkName kind ident Untouched)) env)) names)

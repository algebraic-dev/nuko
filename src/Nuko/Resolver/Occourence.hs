module Nuko.Resolver.Occourence (
  OccName(..),
  NameKind(..),
  OccEnv(..),
  lookupEnv,
  insertEnv,
  updateEnvWith,
  empty,
  findAltKeyValue,
) where

import Relude           (Generic, HashMap, Semigroup, Monoid, Functor (fmap), asum, NonEmpty, Show)
import Relude.Base      (Eq)
import Relude.String    (Text, show)
import Relude.Container (Hashable)
import Relude.Monad     (Maybe, maybe)
import Pretty.Tree      (PrettyTree(..), Tree (Node))

import qualified Data.HashMap.Strict as HashMap

data NameKind
  = VarName
  | TyName
  | ConsName
  | FieldName
  deriving (Eq, Generic, Show)

-- | Name that is not qualified
data OccName = OccName
  { occName :: Text
  , kind :: NameKind
  } deriving (Eq, Generic, Show)

instance PrettyTree NameKind where
instance PrettyTree OccName where
  prettyTree t = Node "OccName" [show t.kind, t.occName] []

instance Hashable NameKind where
instance Hashable OccName where

-- | Useful to store things like visibility of an name inside a module
newtype OccEnv a = OccEnv (HashMap OccName a)
  deriving newtype (Semigroup, Monoid, Functor, Show)

instance PrettyTree a => PrettyTree (OccEnv a) where
  prettyTree (OccEnv a) = prettyTree a

lookupEnv :: OccName -> OccEnv a -> Maybe a
lookupEnv name (OccEnv map') = HashMap.lookup name map'

insertEnv :: OccName -> a -> OccEnv a -> OccEnv a
insertEnv name val (OccEnv map') = OccEnv (HashMap.insert name val map')

updateEnvWith :: OccName -> (a -> a) -> a -> OccEnv a -> OccEnv a
updateEnvWith name update cur (OccEnv env) =
  let val = maybe cur update (HashMap.lookup name env) in
  OccEnv (HashMap.insert name val env)

empty :: OccEnv a
empty = OccEnv HashMap.empty

-- Specifics

findAltKeyValue :: NonEmpty OccName -> OccEnv a -> Maybe (OccName, a)
findAltKeyValue names env = asum (fmap (\name -> fmap (name, ) (lookupEnv name env)) names)

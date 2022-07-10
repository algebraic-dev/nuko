module Nuko.Resolver.Environment (
  NameSort(..),
  Visibility(..),
  NameSpace(..),
  Label(..),
  LocalNS(..),
  Qualified(..),
  joinLabels,
  localNames,
  openedNames,
  scopeLocals,
  setUsage,
  getLocal,
  addLocal,
  openName,
  openedModules,
  addModule,
  getPublicNames,
  currentNamespace,
  scopeNameSpace,
  addDef,
  modName,
  names,
  newNamespaces,
  emptyLocalNS,
) where


import Nuko.Resolver.Occourence (OccEnv (OccEnv), empty, OccName(..), lookupEnv, insertEnv, updateEnvWith)
import Nuko.Report.Range        (Range(..))

import Relude.String            (Text)
import Relude.Container         (HashSet, HashMap, Hashable)
import Relude.Base              (Eq((==)))
import Relude.Bool              (otherwise, Bool (..))
import Relude.Monoid            ((<>))
import Relude.Applicative       (Applicative(pure))
import Relude.Monad             (modify, MonadState, fromMaybe, gets)
import Relude                   (One (one), (.), snd, fst, Maybe (..), (<$>), ($), filter, Functor (fmap), Show, Generic)

import Lens.Micro.Platform      (makeLenses, over, set)
import Data.List.NonEmpty       (NonEmpty ((:|)), (<|), uncons)
import Pretty.Tree              (PrettyTree)

import qualified Data.HashSet             as HashSet
import qualified Data.HashMap.Strict      as HashMap
import qualified Nuko.Resolver.Occourence as Occ

data NameSort
  = External Text
  | Internal

data Visibility
  = Public
  | Private
  deriving (Eq, Show, Generic)

data Qualified = Qualified Text Text deriving (Generic, Eq, Show)

instance Hashable Qualified where

data NameSpace = NameSpace
  { _modName :: Text
  , _names   :: OccEnv Visibility
  } deriving (Show, Generic)

instance PrettyTree Qualified where
instance PrettyTree Visibility where
instance PrettyTree NameSpace where

makeLenses ''NameSpace

data Label
   = Single Qualified
   | Ambiguous (HashSet Qualified)
   deriving Generic

instance PrettyTree Label where

joinLabels :: Label -> Label -> Label
joinLabels a' b' = case (a', b') of
  (Single r, Ambiguous rf)    -> Ambiguous (HashSet.insert r rf)
  (Ambiguous rf, Single r)    -> Ambiguous (HashSet.insert r rf)
  (Ambiguous r, Ambiguous r') -> Ambiguous (r <> r')
  (Single r, Single r')
    | r == r'   -> Single r
    | otherwise -> Ambiguous (HashSet.fromList [r, r'])

-- | LocalScope describes a single scope that stores if it's used
-- and where it's the defined.

type LocalScope = OccEnv (NonEmpty (Range, Bool))

-- _localNames is dumb, i should change it to MonadReader with warning creator at
-- the end.

data LocalNS = LocalNS
  { _openedNames      :: OccEnv Label
  , _localNames       :: NonEmpty LocalScope
  , _openedModules    :: HashMap Text NameSpace
  , _currentNamespace :: NameSpace
  , _newNamespaces    :: HashMap Text NameSpace
  } deriving Generic

instance PrettyTree LocalNS where

makeLenses ''LocalNS

emptyNS :: Text -> NameSpace
emptyNS moduleName = NameSpace moduleName Occ.empty

emptyLocalNS :: Text -> LocalNS
emptyLocalNS moduleName =
  LocalNS Occ.empty (one Occ.empty) HashMap.empty (emptyNS moduleName) HashMap.empty

-- Functions to help the localNames because it's a really messy type

scopeLocals :: MonadState LocalNS m => m a -> m a
scopeLocals action = do
  modify (over localNames (empty <|))
  result <- action
  _      <- gets (fst . uncons . _localNames)
  modify (over localNames (fromMaybe (one empty) . snd . uncons))
  pure result

scopeNameSpace :: MonadState LocalNS m => Text -> Text -> m a -> m a
scopeNameSpace singleName name action = do
  last <- gets _currentNamespace
  modify (set currentNamespace (NameSpace name empty))
  res <- action
  modify (\s -> s { _currentNamespace = last
                  , _newNamespaces = HashMap.insert name s._currentNamespace s._newNamespaces
                  , _openedModules = HashMap.insert singleName s._currentNamespace s._openedModules })
  pure res

setUsage :: LocalNS -> OccName -> Bool -> Maybe LocalNS
setUsage lns@LocalNS { _localNames = (scope :| names') } name bool =
    case setInRest (scope : names') of
      Just (x : xs) -> Just lns { _localNames = x :| xs }
      _             -> Nothing
  where
    setInRest :: [LocalScope] -> Maybe [LocalScope]
    setInRest = \case
      [] -> Nothing
      (scope' : xs) ->
        case lookupEnv name scope' of
          Just ((range, _) :| rest) -> Just (insertEnv name ((range, bool) :| rest) scope' : xs)
          Nothing                   -> (scope' :) <$> setInRest xs

getLocal :: LocalNS -> OccName -> Maybe (Range, Bool)
getLocal LocalNS { _localNames = (scope :| names') } name =
    getInRest (scope : names')
  where
    getInRest :: [LocalScope] -> Maybe (Range, Bool)
    getInRest = \case
      [] -> Nothing
      (scope' : xs) ->
        case lookupEnv name scope' of
          Just (res :| _) -> Just res
          Nothing -> getInRest xs

addLocal :: MonadState LocalNS m => Range -> OccName -> m ()
addLocal range name = do
  (scope :| names') <- gets _localNames
  modify (set localNames (updateEnvWith name ((range, False) <|) (one (range, False)) scope :| names'))

openName :: MonadState LocalNS m => OccName -> Text -> Text -> m ()
openName name modName' ref =
  let joinOp = joinLabels (Single (Qualified ref modName'))
  in modify (over openedNames (updateEnvWith name joinOp (Single (Qualified ref modName'))))

addDef :: MonadState LocalNS m => OccName -> Visibility -> m ()
addDef name vs = modify (over (currentNamespace . names) (insertEnv name vs))

addModule :: MonadState LocalNS m => Text -> NameSpace -> m ()
addModule name space = modify (over openedModules (HashMap.insert name space))

getPublicNames :: OccEnv Visibility -> [OccName]
getPublicNames (OccEnv map) =
    fmap fst
  $ filter (\(_,v) -> v == Public)
  $ HashMap.toList map
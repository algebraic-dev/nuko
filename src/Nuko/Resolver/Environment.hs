module Nuko.Resolver.Environment (
  NameSort(..),
  Visibility(..),
  NameSpace(..),
  Label(..),
  LocalNS(..),
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
) where

import Nuko.Resolver.Occourence (OccEnv (OccEnv), empty, OccName(..), lookupEnv, insertEnv, updateEnvWith)

import Relude.String            (Text)
import Relude.Container         (HashSet, HashMap)
import Relude.Base              (Eq((==)))
import Relude.Bool              (otherwise, Bool (..), (&&))
import Relude.Monoid            ((<>))
import Relude.Applicative       (Applicative(pure))
import Relude.Monad             (modify, MonadState (get, state), fromMaybe, gets)
import Relude                   (One (one), (.), snd, fst, Maybe (..), (<$>), ($), filter, Functor (fmap))

import Lens.Micro.Platform      (makeLenses, over, set)
import Data.List.NonEmpty       (NonEmpty ((:|)), (<|), uncons)
import Nuko.Syntax.Range        (Range(..))

import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap

data NameSort
  = External Text
  | Internal

data Visibility
  = Public
  | Private
  deriving Eq

data NameSpace = NameSpace
  { _modName :: Text
  , _names   :: OccEnv Visibility
  }

makeLenses ''NameSpace

data Label = Single Text Text | Ambiguous (HashSet Text) (HashSet Text)

joinLabels :: Label -> Label -> Label
joinLabels a' b' = case (a', b') of
  (Single r a, Ambiguous rf b)    -> Ambiguous (HashSet.insert r rf) (HashSet.insert a b)
  (Ambiguous rf b, Single r a)    -> Ambiguous (HashSet.insert r rf) (HashSet.insert a b)
  (Ambiguous r a, Ambiguous r' b) -> Ambiguous (r <> r') (a <> b)
  (Single r a, Single r' b)
    | a == b && r == r' -> Single r a
    | otherwise         -> Ambiguous (HashSet.fromList [r, r']) (HashSet.fromList [a,b])

-- | LocalScope describes a single scope that stores if it's used
-- and where it's the defined.

type LocalScope = OccEnv (NonEmpty (Range, Bool))

data LocalNS = LocalNS
  { _openedNames      :: OccEnv Label
  , _localNames       :: NonEmpty LocalScope
  , _openedModules    :: HashMap Text NameSpace
  , _currentNamespace :: NameSpace
  , _newNamespaces    :: HashMap Text NameSpace
  }

makeLenses ''LocalNS

-- Functions to help the localNames because it's a really messy type

scopeLocals :: MonadState LocalNS m => m a -> m a
scopeLocals action = do
  modify (over localNames (empty <|))
  result <- action
  scope  <- gets (fst . uncons . _localNames)
  modify (over localNames (fromMaybe (one empty) . snd . uncons))
  pure result

scopeNameSpace :: MonadState LocalNS m => Text -> m a -> m a
scopeNameSpace name action = do
  last <- gets _currentNamespace
  modify (set currentNamespace (NameSpace name empty))
  res <- action
  modify (\s -> s { _currentNamespace = last, _newNamespaces = HashMap.insert name s._currentNamespace s._newNamespaces })
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
openName name modName' ref = modify (over openedNames (updateEnvWith name (joinLabels (Single ref modName')) (Single ref modName')))

addDef :: MonadState LocalNS m => OccName -> Visibility -> m ()
addDef name vs = modify (over (currentNamespace . names) (insertEnv name vs))

addModule :: MonadState LocalNS m => Text -> NameSpace -> m ()
addModule name space = modify (over openedModules (HashMap.insert name space))

getPublicNames :: OccEnv Visibility -> [OccName]
getPublicNames (OccEnv map) =
    fmap fst
  $ filter (\(_,v) -> v == Public)
  $ HashMap.toList map
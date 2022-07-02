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
  resolveName
) where

import Nuko.Resolver.Occourence (OccEnv, empty, OccName(..), lookupEnv, insertEnv, updateEnvWith)
import Nuko.Resolver.Error      (ResolveError (AmbiguousNames))
import Nuko.Resolver.Tree       (Path (Local, Path), ReId (ReId))

import Relude.String            (Text)
import Relude.Container         (HashSet)
import Relude.Base              (Eq((==)))
import Relude.Bool              (otherwise, Bool (..))
import Relude.Monoid            ((<>))
import Relude.Applicative       (Applicative(pure))
import Relude.Monad             (modify, MonadState, fromMaybe, gets)
import Relude                   (One (one), (.), snd, fst, Maybe (..), (<$>), Either (..), MonadState (get, put), Applicative ((*>)), ($))

import Lens.Micro.Platform      (makeLenses, over)
import Data.List.NonEmpty       (NonEmpty ((:|)), (<|), uncons)
import Nuko.Syntax.Range        (Range(..))

import qualified Data.HashSet as HashSet

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

-- | LocalScope describes a single scope that stores if it's used
-- and where it's the defined.

type LocalScope = OccEnv (NonEmpty (Range, Bool))

data LocalNS = LocalNS
  { _openedNames :: OccEnv Label
  , _localNames  :: NonEmpty LocalScope
  }

makeLenses ''LocalNS

-- Functions to help the localNames because it's a really messy type

scopeLocals :: MonadState LocalNS m => m a -> m (a, LocalScope)
scopeLocals action = do
    modify (over localNames (empty <|))
    result <- action
    scope  <- gets (fst . uncons . _localNames)
    modify (over localNames (fromMaybe (one empty) . snd . uncons))
    pure (result, scope)

setUsage :: LocalNS -> OccName -> Bool -> Maybe LocalNS
setUsage lns@LocalNS { _localNames = (scope :| names) } name bool =
    case setInRest (scope : names) of
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
getLocal LocalNS { _localNames = (scope :| names) } name =
    getInRest (scope : names)
  where
    getInRest :: [LocalScope] -> Maybe (Range, Bool)
    getInRest = \case
      [] -> Nothing
      (scope' : xs) ->
        case lookupEnv name scope' of
          Just (res :| _) -> Just res
          Nothing -> getInRest xs

addLocal :: LocalNS -> Range -> OccName -> LocalNS
addLocal lns@LocalNS { _localNames = (scope :| names) } range name =
  lns { _localNames = (updateEnvWith name ((range, False) <|) (one (range, False)) scope) :| names }

resolveName :: MonadState LocalNS m => Range -> OccName -> (Maybe Text -> Text -> Range -> ResolveError) -> m (Either ResolveError Path)
resolveName range name err = do
  env <- get
  case setUsage env name True of
    Just newEnv -> put newEnv *> pure (Right (Local (ReId name.name range)))
    Nothing -> pure $
      case lookupEnv name env._openedNames of
        Just (Ambiguous other) -> Left  (AmbiguousNames other)
        Just (Single mod')     -> Right (Path mod' (ReId name.name range) range)
        Nothing                -> Left  (err Nothing name.name range)

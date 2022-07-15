module Nuko.Resolver.Env (
  ResolverState(..),
  Visibility(..),
  NameSpace(..),
  Query(..),
  Use(..),
  MonadResolver,
  ImportErrorKind(..),
  useModule,
  modName,
  names,
  openedModules,
  openedNames,
  currentNamespace,
  localNames,
  joinUses,
  emptyNamespace,
  newNamespace,
  emptyState,
  newScope,
  addDefinition,
  addModule,
  openName,
  newLocal,
  useLocal,
  addGlobal
) where

import Nuko.Resolver.Occourence (OccEnv, empty, insertOcc, insertWith, getMap)
import Nuko.Report.Range        (Range, HasPosition (..))
import Nuko.Resolver.Error      (ResolveError (..), mkErr, ResolveErrorReason (..))
import Nuko.Utils               (terminate)
import Nuko.Names               (Qualified, ModName, Label(..), Ident(..), Name(..), changeName, getChildName, mkIdent)

import Relude.Container         (HashSet, HashMap)
import Relude.Base              (Eq((==)))
import Relude.Bool              (otherwise)
import Relude.Monoid            ((<>), Endo)
import Relude.Applicative       (Applicative(pure))
import Relude.Monad             (MonadState, maybe)
import Relude                   (Show, Generic, (.), Text, Num ((+)), show, Int, const, Maybe (..), ($), Either)

import Pretty.Tree              (PrettyTree)
import Control.Monad.Chronicle  (MonadChronicle)
import Control.Monad.Query     (MonadQuery)

import Lens.Micro.Platform      (makeLenses, use, (.=), (%=), at)

import qualified Data.HashSet             as HashSet
import qualified Data.HashMap.Strict      as HashMap
import qualified Nuko.Resolver.Occourence as Occ

data Visibility
  = Public
  | Private
  deriving (Eq, Show, Generic)

data Use
   = Single (Qualified Label)
   | Ambiguous (HashSet (Qualified Label))
   deriving Generic

data NameSpace = NameSpace
  { _modName :: ModName
  , _names   :: OccEnv Visibility
  } deriving Generic

-- | Describes the current state of the resolver
data ResolverState = ResolverState
  { _openedNames      :: OccEnv Use
  -- ^ All the names that are available that are external
  , _openedModules    :: HashMap ModName NameSpace
  -- ^ All the modules available for the current module
  , _localNames       :: OccEnv Label
  , _usedLocals       :: OccEnv Range
  -- ^ Used to tracking locals.
  , _currentNamespace :: NameSpace
  , _newNamespaces    :: HashMap ModName NameSpace
  } deriving Generic

makeLenses ''NameSpace
makeLenses ''ResolverState

instance PrettyTree Visibility where
instance PrettyTree Use where
instance PrettyTree NameSpace where
instance PrettyTree ResolverState where

data ImportErrorKind
  = CannotFind
  | Cyclic

data Query a where
  GetModule :: ModName -> Query (Either ImportErrorKind NameSpace)

type MonadResolver m =
  ( MonadQuery Query m
  , MonadState ResolverState m
  , MonadChronicle (Endo [ResolveError]) m
  )

joinUses :: Use -> Use -> Use
joinUses a' b' = case (a', b') of
  (Single r, Ambiguous rf)    -> Ambiguous (HashSet.insert r rf)
  (Ambiguous rf, Single r)    -> Ambiguous (HashSet.insert r rf)
  (Ambiguous r, Ambiguous r') -> Ambiguous (r <> r')
  (Single r, Single r')
    | r == r'   -> Single r
    | otherwise -> Ambiguous (HashSet.fromList [r, r'])

emptyNamespace :: ModName -> NameSpace
emptyNamespace moduleName = NameSpace moduleName Occ.empty

-- | This function is useful to create namespaces for types.
newNamespace :: MonadState ResolverState m => ModName -> m a -> m a
newNamespace newModuleName action = do

  lastNamespace    <- use currentNamespace
  currentNamespace .= NameSpace newModuleName empty
  result           <- action
  resultNamespace  <- use currentNamespace
  currentNamespace .= lastNamespace
  newNamespaces    %= HashMap.insert newModuleName resultNamespace

  -- Probably a bad decision but in the future i'll fix that :P
  openedModules    %= HashMap.insert newModuleName resultNamespace
  openedModules    %= HashMap.insert (getChildName newModuleName) resultNamespace

  pure result

emptyState :: ModName -> ResolverState
emptyState moduleName = ResolverState Occ.empty HashMap.empty Occ.empty Occ.empty (emptyNamespace moduleName) HashMap.empty

newScope :: MonadState ResolverState m => m a -> m a
newScope action = do
  locals     <- use localNames
  used       <- use usedLocals
  result     <- action
  -- Here i'll try to throw some warnings about names that were not used.
  localNames .= locals
  usedLocals .= used
  pure result

addDefinition :: MonadState ResolverState m => Label -> Visibility -> m ()
addDefinition name vs = currentNamespace . names %= insertOcc name vs

addModule :: MonadState ResolverState m => ModName -> NameSpace -> m ()
addModule name space = openedModules %= HashMap.insert name space

openName :: MonadState ResolverState m => Label -> Qualified Label -> m ()
openName name label = openedNames %= insertWith name joinUses (Single label)

newLocal :: MonadState ResolverState m => Name k -> m (Name k)
newLocal lName = do
    locals      <- use localNames
    let text     = lName.nIdent.iText
    let newLabel = maybe lName (const (generateNewName locals lName text 1)) (Occ.lookupOcc (Label lName) locals)
    localNames  %= Occ.insertOcc (Label lName) (Label newLabel)
    localNames  %= Occ.insertOcc (Label newLabel) (Label newLabel)
    pure newLabel
  where
    generateNewName :: OccEnv a -> Name k -> Text -> Int -> Name k
    generateNewName used lName' name count =
      let newName = changeName (mkIdent (name <> "_" <> show count) (getPos lName')) lName' in
      if Occ.member (Label newName) used
        then generateNewName used lName' name (count + 1)
        else newName

useLocal :: MonadResolver m => Label -> m ()
useLocal label = do
    local <- use (localNames . getMap . at label)
    case local of
      Nothing -> terminate (mkErr $ AlreadyExistsName label)
      Just _  -> usedLocals %= Occ.insertOcc label (getPos label)

useModule :: MonadResolver m => ModName -> m NameSpace
useModule modName' = do
    moduleRes <- use (openedModules . at modName')
    case moduleRes of
      Nothing  -> terminate (mkErr $ CannotFindModule modName')
      Just res -> pure res

addGlobal :: MonadResolver m => Name k -> Visibility -> m ()
addGlobal name vs = do
  result <- use (currentNamespace . names . getMap . at (Label name))
  case result of
    Just _  -> terminate (mkErr $ AlreadyExistsName (Label name))
    Nothing -> addDefinition (Label name) vs
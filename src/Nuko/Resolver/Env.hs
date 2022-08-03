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
  addGlobal,
  mkDiagnostic
) where

import Relude

import Nuko.Names               (Ident (..), Label (..), ModName, Name (..),
                                 Qualified, changeName, getChildName, mkIdent)
import Nuko.Report.Range        (HasPosition (..), Range)
import Nuko.Resolver.Error      (ResolveErrorReason (..))
import Nuko.Resolver.Occourence (OccEnv, getMap, insertOcc, insertWith)
import Nuko.Utils               (flag, terminate)
import Pretty.Tree              (PrettyTree)

import Control.Monad.Chronicle  (MonadChronicle)
import Control.Monad.Query      (MonadQuery)
import Lens.Micro.Platform      (at, makeLenses, use, (%=), (.=))

import Data.HashMap.Strict      qualified as HashMap
import Data.HashSet             qualified as HashSet
import Nuko.Report.Message      qualified as Message
import Nuko.Resolver.Occourence qualified as Occ

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
  , _reFilename       :: Text
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
  , MonadChronicle (Endo [Message.Diagnostic]) m
  )

mkDiagnostic :: MonadResolver m => Message.Severity -> Range -> ResolveErrorReason -> m Message.Diagnostic
mkDiagnostic severity range reason = do
  namespace <- use (currentNamespace . modName)
  filename  <- use reFilename
  pure (Message.Diagnostic namespace filename severity range (Message.ResolveError reason))

joinUses :: Use -> Use -> Use
joinUses a' b' = case (a', b') of
  (Single r, Ambiguous rf)    -> Ambiguous (HashSet.insert r rf)
  (Ambiguous rf, Single r)    -> Ambiguous (HashSet.insert r rf)
  (Ambiguous r, Ambiguous r') -> Ambiguous (r <> r')
  (Single r, Single r')
    | r == r'   -> Single r
    | otherwise -> Ambiguous (HashSet.fromList [r, r'])

emptyNamespace :: ModName -> NameSpace
emptyNamespace moduleName = NameSpace moduleName Occ.emptyOcc

-- | This function is useful to create namespaces for types.
newNamespace :: MonadState ResolverState m => ModName -> m a -> m a
newNamespace newModuleName action = do

  lastNamespace    <- use currentNamespace
  currentNamespace .= NameSpace newModuleName mempty
  result           <- action
  resultNamespace  <- use currentNamespace
  currentNamespace .= lastNamespace
  newNamespaces    %= HashMap.insert newModuleName resultNamespace

  -- Probably a bad decision but in the future i'll fix that :P
  openedModules    %= HashMap.insert newModuleName resultNamespace
  openedModules    %= HashMap.insert (getChildName newModuleName) resultNamespace

  pure result

emptyState :: ModName -> Text -> ResolverState
emptyState moduleName = ResolverState Occ.emptyOcc HashMap.empty Occ.emptyOcc Occ.emptyOcc (emptyNamespace moduleName) HashMap.empty

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
newLocal localName = do
    locals      <- use localNames
    let text     = localName.nIdent.iText
    let newLabel = maybe localName (const (generateNewName locals localName text 1)) (Occ.lookupOcc (Label localName) locals)
    localNames  %= Occ.insertOcc (Label localName) (Label newLabel)
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
    localLabel <- use (localNames . getMap . at label)
    case localLabel of
      Nothing -> flag =<< mkDiagnostic Message.Error (getPos label) (AlreadyExistsName label)
      Just _  -> usedLocals %= Occ.insertOcc label (getPos label)

useModule :: MonadResolver m => ModName -> m NameSpace
useModule modName' = do
    moduleRes <- use (openedModules . at modName')
    case moduleRes of
      Nothing  -> terminate =<< mkDiagnostic Message.Error (getPos modName')  (CannotFindModule modName')
      Just res -> pure res

addGlobal :: MonadResolver m => Name k -> Visibility -> m ()
addGlobal name vs = do
  result <- use (currentNamespace . names . getMap . at (Label name))
  case result of
    Just _  -> flag =<< mkDiagnostic Message.Error (getPos name) (AlreadyExistsName (Label name))
    Nothing -> addDefinition (Label name) vs

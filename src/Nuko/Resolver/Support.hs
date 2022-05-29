module Nuko.Resolver.Support (
    ImportResult(..),
    MonadImport(..),
    Resolution(..),
    Module(..),
    Env(..),
    moduleName,
    valueDecls,
    tyDecls,
    consDecls,
    fieldDecls,
    emptyMod,
    emptyEnv,
    localBindings,
    aliasedModules,
    openedModules,
    importedModules,
    currentModule,
    recImp
) where

import Control.Monad.State   (MonadTrans (lift), StateT)
import Control.Monad.Reader  (ReaderT)
import Control.Monad.Except  (ExceptT)
import Data.HashMap.Strict   (HashMap)
import Data.Text             (Text)
import Data.HashSet          (HashSet)
import Data.List.NonEmpty    (NonEmpty)
import Lens.Micro.TH         (makeLenses)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

newtype Resolution = Resolution (NonEmpty Text) deriving Show

data Module = Module
  { _moduleName :: Text
  , _valueDecls :: HashMap Text Resolution
  , _tyDecls    :: HashMap Text Resolution
  , _consDecls  :: HashMap Text Resolution
  , _fieldDecls :: HashMap Text Resolution
  } deriving Show

data Env = Env
  { _localBindings   :: HashSet Text
  , _aliasedModules  :: HashMap Text Text
  , _openedModules   :: HashMap Text Module
  , _importedModules :: HashMap Text Module
  , _currentModule   :: Module
  }

makeLenses ''Module
makeLenses ''Env

emptyMod :: Text -> Module
emptyMod text = Module text HashMap.empty HashMap.empty HashMap.empty HashMap.empty

emptyEnv :: Module -> Env
emptyEnv = Env HashSet.empty HashMap.empty HashMap.empty HashMap.empty

data ImportResult s
    = Succeded s
    | NotFound
    deriving stock Show

recImp :: (s -> b) -> b -> ImportResult s -> b
recImp fn _   (Succeded a) = fn a
recImp _  alt NotFound     = alt

class Monad m => MonadImport s m | m -> s where
    importModule   :: Text      -> m (ImportResult s)
    addModule      :: Text -> s -> m (ImportResult s)

instance MonadImport n m => MonadImport n (ReaderT s m) where
    importModule = lift . importModule
    addModule k  = lift . addModule k

instance MonadImport n m => MonadImport n (ExceptT s m) where
    importModule = lift . importModule
    addModule k  = lift . addModule k

instance MonadImport n m => MonadImport n (StateT s m) where
    importModule = lift . importModule
    addModule k  = lift . addModule k

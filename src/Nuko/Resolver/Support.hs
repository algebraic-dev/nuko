module Nuko.Resolver.Support (
    ImportResult(..),
    MonadImport(..)
) where

import Data.Text (Text)
import Control.Monad.State (StateT, MonadTrans (lift))
import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (ExceptT)

data ImportResult s
    = Succeded s
    | NotFound

class Monad m => MonadImport s m | m -> s where
    importModule   :: Text      -> m (ImportResult s)
    addModule      :: Text -> s -> m (ImportResult s)

instance MonadImport n m => MonadImport n (ReaderT s m) where
    importModule = lift . importModule
    addModule k  = lift . addModule k

instance MonadImport n m => MonadImport n (ExceptT s m) where
    importModule = lift . importModule
    addModule k  = lift . addModule k

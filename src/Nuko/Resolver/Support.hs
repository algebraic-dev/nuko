module Nuko.Resolver.Support (
    ImportResult(..),
    MonadImport(..)
) where

import Data.Text (Text)
import Control.Monad.State (StateT, MonadTrans (lift))
import Control.Monad.Reader (ReaderT)

data ImportResult s
    = Succeded s
    | NotFound

class Monad m => MonadImport s m | m -> s where
    importModule   :: Text -> m (ImportResult s)
    stackNamespace :: Text -> (m a -> m a) -> m a

instance MonadImport n m => MonadImport n (StateT s m) where
    importModule = lift . importModule

instance MonadImport n m => MonadImport n (ReaderT s m) where
    importModule = lift . importModule
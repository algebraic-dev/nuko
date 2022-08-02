module Control.Monad.Query (
  MonadQuery(..),
) where

import Relude

class (Monad m) => MonadQuery q m | m -> q where
  query :: q a -> m a

instance MonadQuery q m => MonadQuery q (StateT s m) where
  query t = lift (query t)

instance MonadQuery q m => MonadQuery q (ReaderT s m) where
  query t = lift (query t)

instance MonadQuery q m => MonadQuery q (ExceptT s m) where
  query t = lift (query t)

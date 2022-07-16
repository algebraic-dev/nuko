module Control.Monad.Query (
  MonadQuery(..),
) where

import Relude.Monad       (Monad(..), StateT, ReaderT, ExceptT)
import Relude             (MonadTrans (lift))

class (Monad m) => MonadQuery q m | m -> q where
  query :: q a -> m a

--  Could not deduce (MonadQuery NameSpace (State.StateT LocalNS m))

instance MonadQuery q m => MonadQuery q (StateT s m) where
  query t = lift (query t)

instance MonadQuery q m => MonadQuery q (ReaderT s m) where
  query t = lift (query t)

instance MonadQuery q m => MonadQuery q (ExceptT s m) where
  query t = lift (query t)
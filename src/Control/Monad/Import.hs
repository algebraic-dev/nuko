module Control.Monad.Import (
  ImportErrorKind(..),
  MonadImport(..),
) where

import Relude.Monad       (Monad(..), Either, StateT, ReaderT, ExceptT)
import Relude             (Text, MonadTrans (lift))

data ImportErrorKind
  = CannotFind
  | Cyclic

class (Monad m) => MonadImport r m | m -> r where
  importIn :: Text -> m (Either ImportErrorKind r)
  addIn    :: Text -> r -> m ()

--  Could not deduce (MonadImport NameSpace (State.StateT LocalNS m))

instance MonadImport e m => MonadImport e (StateT s m) where
  importIn t = lift (importIn t)
  addIn f t  = lift (addIn f t)

instance MonadImport e m => MonadImport e (ReaderT s m) where
  importIn t = lift (importIn t)
  addIn f t  = lift (addIn f t)

instance MonadImport e m => MonadImport e (ExceptT s m) where
  importIn t = lift (importIn t)
  addIn f t  = lift (addIn f t)
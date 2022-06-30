module Control.Monad.Import (
  ImportErrorKind(..),
  MonadImport(..),
) where

import Relude (Monad, Text, Either, StateT, MonadTrans (lift), ReaderT, ExceptT)

data ImportErrorKind
  = CannotFind Text
  | Cyclic

class (Monad m) => MonadImport r m | m -> r where
  importIn :: Text -> m (Either ImportErrorKind r)
  addIn    :: Text -> r -> m ()

instance MonadImport e m => MonadImport e (StateT s m) where
  importIn t = lift (importIn t)
  addIn f t  = lift (addIn f t)

instance MonadImport e m => MonadImport e (ReaderT s m) where
  importIn t = lift (importIn t)
  addIn f t  = lift (addIn f t)

instance MonadImport e m => MonadImport e (ExceptT s m) where
  importIn t = lift (importIn t)
  addIn f t  = lift (addIn f t)
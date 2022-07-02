module Resolver.PreludeImporter (
  runResolverNull,
) where


import Nuko.Resolver.Environment (NameSpace, LocalNS(..), Label(..), emptyLocalNS)
import Nuko.Resolver.Occourence  (NameKind(..), OccName(..), insertEnv)
import Nuko.Resolver.Error       (ResolveError)
import Nuko.Resolver.Types       (MonadResolver)
import Control.Monad.Import      (MonadImport (importIn, addIn), ImportErrorKind (CannotFind))
import Control.Monad.Chronicle   (MonadChronicle(..))
import Relude.Monad              (Monad ((>>=)), MonadState (..))
import Relude.Monoid             (Endo(appEndo))
import Relude.Functor            (Functor, fmap, (<$>), first)
import Relude.Applicative        (Applicative(pure, (<*>)))
import Relude                    (($), Either (..), (.))
import Data.These                (These)

import qualified Control.Monad.State as State
import qualified Control.Monad.Chronicle as Chronicle
import qualified Nuko.Resolver.Occourence as Occ

data TestImport m s = TestImport { runNull :: m s }

instance Monad m => Functor (TestImport m) where fmap f (TestImport s) = (TestImport (f <$> s))

instance Monad m => Applicative (TestImport m) where
  pure s    = TestImport (pure s)
  (<*>) a b = TestImport ((runNull a) <*> (runNull b))

instance Monad m => Monad (TestImport m) where (>>=) b a = TestImport ((a <$> (runNull b)) >>= runNull)

instance Monad m => MonadImport NameSpace (TestImport m) where
  importIn _ = TestImport (pure $ Left CannotFind)
  addIn _ _  = pure ()

instance MonadState s m => MonadState s (TestImport m) where
  state = TestImport . state

instance MonadChronicle s m => MonadChronicle s (TestImport m) where
  dictate     = TestImport . dictate
  confess     = TestImport . confess
  absolve a f = TestImport (absolve a (runNull f))
  retcon  s f = TestImport (retcon s (runNull f))
  memento f   = TestImport $ memento (runNull f)
  condemn f   = TestImport $ condemn (runNull f)
  chronicle f = TestImport $ chronicle f


runResolverNull :: (forall m . MonadResolver m => m a) -> These [ResolveError] (a, LocalNS)
runResolverNull action =
    let localNS  = (emptyLocalNS "Main") { _openedNames = openedLs}
        openedLs = insertEnv (OccName "Int" TyName)    (Single "Int" "Prelude")
                 $ insertEnv (OccName "String" TyName) (Single "String" "Prelude")
                 $ Occ.empty
    in first
       (`appEndo` [])
       (Chronicle.runChronicle $ State.runStateT (runNull action) localNS)
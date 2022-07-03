{-# OPTIONS_GHC -Wno-orphans #-}
module Resolver.PreludeImporter (
  resolveEntireProgram,
) where


import Nuko.Resolver.Environment (NameSpace, LocalNS(..), Label(..), emptyLocalNS)
import Nuko.Resolver.Occourence  (NameKind(..), OccName(..), insertEnv)
import Nuko.Resolver.Error       (ResolveError)
import Nuko.Resolver.Types       (MonadResolver)
import Nuko.Tree                 (Program(Program), Nm, Re)
import Nuko.Resolver             (initProgram, resolveProgram)
import Control.Monad.Import      (MonadImport (importIn), ImportErrorKind (CannotFind))
import Relude.Monad              (Monad((>>=)), Maybe(..))
import Relude.Monoid             (Endo(appEndo))
import Relude.Functor            (first)
import Relude.Applicative        (Applicative(pure))
import Relude                    (($), Either (..), HashMap, Text, Functor, ReaderT, MonadState)
import Data.These                (These)
import Control.Monad.Chronicle   (MonadChronicle)

import qualified Control.Monad.State as State
import qualified Control.Monad.Chronicle as Chronicle
import qualified Nuko.Resolver.Occourence as Occ
import qualified Data.HashMap.Strict as HashMap
import qualified Control.Monad.Reader as Reader

newtype ConstImporter m a = ConstImporter { runImporter :: ReaderT (HashMap Text NameSpace) m a }
  deriving newtype (Functor, Monad, Applicative, MonadState b, MonadChronicle b)

instance Monad m => MonadImport NameSpace (ConstImporter m) where
  importIn name = ConstImporter $ do
    r <- Reader.asks (HashMap.lookup name)
    case r of
      Just res -> pure (Right res)
      Nothing  -> pure (Left CannotFind)

runImporterTo :: ConstImporter m a -> (HashMap Text NameSpace) -> m a
runImporterTo imp map = Reader.runReaderT (runImporter imp) map

runResolver :: (forall m . MonadResolver m => m a) -> LocalNS -> HashMap Text NameSpace -> These [ResolveError] (a, LocalNS)
runResolver action localNS predefined = first (`appEndo` []) (Chronicle.runChronicle $ State.runStateT (runImporterTo action predefined) localNS)

resolveEntireProgram :: Program Nm -> These [ResolveError] (Program Re, LocalNS)
resolveEntireProgram tree =
    let localNS  = (emptyLocalNS "Main") { _openedNames = openedLs}
        openedLs = insertEnv (OccName "Int" TyName)    (Single "Int" "Prelude")
                 $ insertEnv (OccName "String" TyName) (Single "String" "Prelude")
                 $ Occ.empty in
        runResolver (initProgram tree) localNS HashMap.empty
    >>= \(_, initNS) -> runResolver (resolveProgram tree) initNS (initNS._newNamespaces)
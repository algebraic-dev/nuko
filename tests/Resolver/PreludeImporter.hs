{-# OPTIONS_GHC -Wno-orphans #-}
module Resolver.PreludeImporter (
  resolveEntireProgram,
) where


import Nuko.Resolver.Path
import Nuko.Resolver.Env
import Nuko.Resolver.Occourence
import Nuko.Resolver.Error       (ResolveError)
import Nuko.Tree                 (Nm, Re, Program (..))
import Nuko.Resolver             (initProgram, resolveProgram)
import Nuko.Names
import Control.Monad.Import      (MonadImport (importIn), ImportErrorKind (CannotFind))
import Relude.Monad              (Monad((>>=)), Maybe(..))
import Relude.Monoid             (Endo(appEndo))
import Relude.Functor            (first)
import Relude.Applicative        (Applicative(pure))
import Relude                    (($), (<$>), Either (..), HashMap, Text, Functor, ReaderT, MonadState)
import Data.These                (These)
import Control.Monad.Chronicle   (MonadChronicle)
import Relude.List.NonEmpty      (NonEmpty((:|)))
import Data.Text

import qualified Control.Monad.State      as State
import qualified Control.Monad.Chronicle  as Chronicle
import qualified Nuko.Resolver.Occourence as Occ
import qualified Data.HashMap.Strict      as HashMap
import qualified Control.Monad.Reader     as Reader

newtype ConstImporter m a = ConstImporter { runImporter :: ReaderT (HashMap ModName NameSpace) m a }
  deriving newtype (Functor, Monad, Applicative, MonadState b, MonadChronicle b)

instance Monad m => MonadImport NameSpace (ConstImporter m) where
  importIn name = ConstImporter $ do
    let (x : xs) = splitOn "." name
    let modName = mkModName (genIdent <$> (x :| xs))
    r <- Reader.asks (HashMap.lookup modName)
    case r of
      Just res -> pure (Right res)
      Nothing  -> pure (Left CannotFind)

runImporterTo :: ConstImporter m a -> HashMap ModName NameSpace -> m a
runImporterTo imp = Reader.runReaderT (runImporter imp)

runResolver :: (forall m . MonadResolver m => m a) -> ResolverState -> HashMap ModName NameSpace -> These [ResolveError] (a, ResolverState)
runResolver action r p = first (`appEndo` []) (Chronicle.runChronicle $ State.runStateT (runImporterTo action p) r)

intType :: Label
intType = Label $ mkTyName (genIdent "Int")

strType :: Label
strType = Label $ mkTyName (genIdent "String")

preludeMod :: ModName
preludeMod = mkModName (genIdent "Prelude" :| [])

resolveEntireProgram :: Program Nm -> These [ResolveError] (Program Re, ResolverState)
resolveEntireProgram tree =
    let st  = (emptyState (mkModName (genIdent "Main" :| []))) { _openedNames = openedLs}
        openedLs = insertOcc intType (Single (mkQualifiedWithPos preludeMod intType))
                 $ insertOcc strType (Single (mkQualifiedWithPos preludeMod strType))
                   Occ.empty in
        runResolver (initProgram tree) st HashMap.empty
    >>= \(_, initNS) -> runResolver (resolveProgram tree) initNS (initNS._newNamespaces)
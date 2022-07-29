module Nuko.Driver (compile, mainMod) where

import Nuko.Names                (mkQualifiedWithPos, mkTyName, genIdent, mkModName, Label(Label), ModName, mkName, NameKind (TyName), Attribute (..))
import Nuko.Report.Message       (Diagnostic)
import Nuko.Resolver.Occourence  (insertOcc)
import Nuko.Resolver.Env         (emptyState, Use(..), ResolverState(..), MonadResolver, NameSpace, Query(..), ImportErrorKind (..))
import Nuko.Resolver             (initProgram, resolveProgram)
import Nuko.Syntax.Parser        (parseProgram)
import Nuko.Syntax.Lexer.Support (runLexer)
import Nuko.Typer.Infer.Literal  (preludeQual, intTy, strTy, boolTy)
import Nuko.Typer.Infer          (inferProgram)
import Nuko.Typer.Env            (TypeSpace(..), emptyTypeSpace, TyInfo (..), TyInfoKind (..), runToIO, TypingEnv)
import Nuko.Typer.Kinds          (TKind(..))
import Nuko.Tree                 (Program (..), Tc)

import Relude.List.NonEmpty      (NonEmpty((:|)))
import Relude.Functor            (first)
import Relude.Monoid             (Endo(appEndo))
import Relude.Monad              (Monad((>>=)), Maybe(..))
import Relude                    (($), Either (..), HashMap, Functor, ReaderT, MonadState, Text)
import Relude                    (ByteString, Applicative (pure), One (..))

import Data.These                (These (..))
import Control.Monad.Query       (MonadQuery (query))
import Control.Monad.Chronicle   (MonadChronicle)

import qualified Nuko.Resolver.Occourence as Occ
import qualified Control.Monad.State      as State
import qualified Control.Monad.Reader     as Reader
import qualified Control.Monad.Chronicle  as Chronicle
import qualified Data.HashMap.Strict      as HashMap
import GHC.IO (unsafePerformIO)

newtype ConstImporter m a = ConstImporter { runImporter :: ReaderT (HashMap ModName NameSpace) m a }
  deriving newtype (Functor, Monad, Applicative, MonadState b, MonadChronicle b)

instance Monad m => MonadQuery Query (ConstImporter m) where
  query (GetModule modName') = ConstImporter $ do
    r <- Reader.asks (HashMap.lookup modName')
    case r of
      Just res -> pure (Right res)
      Nothing  -> pure (Left CannotFind)

runImporterTo :: ConstImporter m a -> HashMap ModName NameSpace -> m a
runImporterTo imp = Reader.runReaderT (runImporter imp)

runResolver :: (forall m . MonadResolver m => m a) -> ResolverState -> HashMap ModName NameSpace -> These [Diagnostic] (a, ResolverState)
runResolver action r p = first (`appEndo` []) (Chronicle.runChronicle $ State.runStateT (runImporterTo action p) r)

intType :: Label
intType = Label $ mkTyName (genIdent "Int")

strType :: Label
strType = Label $ mkTyName (genIdent "String")

boolType :: Label
boolType = Label $ mkTyName (genIdent "Bool")

preludeMod :: ModName
preludeMod = mkModName (genIdent "Prelude" :| [])

openedPrelude :: Occ.OccEnv Use
openedPrelude =
      insertOcc intType (Single (mkQualifiedWithPos preludeMod intType))
    $ insertOcc strType (Single (mkQualifiedWithPos preludeMod strType))
    $ insertOcc boolType (Single (mkQualifiedWithPos preludeMod boolType))
      Occ.empty

mainMod :: ModName
mainMod = mkModName (one $ genIdent "Main")

commonState :: Text -> ResolverState
commonState filename = (emptyState mainMod filename) { _openedNames = openedPrelude }

prelude :: TypeSpace
prelude = emptyTypeSpace
  { _tsTypes = HashMap.fromList
    [ (preludeQual "Int", (KiStar, TyInfo intTy (mkName TyName (genIdent "Int") Untouched) [] IsBuiltIn))
    , (preludeQual "String", (KiStar, TyInfo strTy (mkName TyName (genIdent "String") Untouched) [] IsBuiltIn))
    , (preludeQual "Bool", (KiStar, TyInfo boolTy (mkName TyName (genIdent "Bool") Untouched) [] IsBuiltIn))
    ]
  }

compile :: ModName -> Text -> ByteString -> These [Diagnostic] (Program Tc, TypingEnv)
compile modName' filename content =
  runLexer parseProgram modName' filename content
  >>= \program  -> runResolver (initProgram program) (commonState filename) HashMap.empty
  >>= \(_, ns)  -> runResolver (resolveProgram program) ns (ns._newNamespaces)
  >>= \(res,_) -> unsafePerformIO (runToIO prelude modName' filename (inferProgram res))
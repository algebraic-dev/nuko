module Nuko.Driver (compile, mainMod) where

import Relude

import Nuko.Names                (Attribute (..), Label (Label), ModName,
                                  NameKind (TyName), genIdent, mkModName,
                                  mkName, mkQualifiedWithPos, mkTyName)
import Nuko.Report.Message       (Diagnostic)
import Nuko.Resolver             (initProgram, resolveProgram)
import Nuko.Resolver.Env         (ImportErrorKind (..), MonadResolver,
                                  NameSpace, Query (..), ResolverState (..),
                                  Use (..), emptyState)
import Nuko.Resolver.Occourence  (insertOcc)
import Nuko.Syntax.Lexer.Support (runLexer)
import Nuko.Syntax.Parser        (parseProgram)

import Nuko.Tree                 (Program (..), Tc)
import Nuko.Typer.Env            (TyInfo (..), TyInfoKind (..), TypeSpace (..),
                                  TypingEnv, emptyTypeSpace, runToIO)
import Nuko.Typer.Infer          (inferProgram)
import Nuko.Typer.Infer.Literal  (boolTy, intTy, preludeQual, strTy)
import Nuko.Typer.Kinds          (TKind (..))

import Control.Monad.Chronicle   (MonadChronicle)
import Control.Monad.Query       (MonadQuery (query))
import Data.These                (These (..))
import GHC.IO                    (unsafePerformIO)

import Control.Monad.Chronicle   qualified as Chronicle
import Control.Monad.Reader      qualified as Reader
import Control.Monad.State       qualified as State
import Data.HashMap.Strict       qualified as HashMap
import Nuko.Resolver.Occourence  qualified as Occ

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
      Occ.emptyOcc

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

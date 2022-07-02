module Nuko.Resolver.Types (
  MonadResolver,
  makePath,
  resolveName,
  addGlobal,
  resolvePath,
  findInSpace,
) where

import Nuko.Resolver.Occourence  (lookupEnv, OccName(..), NameKind, findAltKeyValue)
import Nuko.Resolver.Environment (LocalNS(..), NameSpace(..), Label (..), setUsage, Visibility (Private, Public), currentNamespace, names, addDef)
import Nuko.Resolver.Error       (ResolveError (..))
import Nuko.Resolver.Tree        (Path (Local, Path), ReId(..))
import Nuko.Tree.TopLevel        (ImpPath)
import Nuko.Syntax.Range         (Range(..))
import Nuko.Syntax.Tree          (Name(..))
import Nuko.Utils                (terminate)
import Nuko.Tree (Nm, Re, XPath)

import Relude.Applicative        (Applicative(pure))
import Relude.Monad              (MonadState (get, put), Maybe (..), fromMaybe, gets)
import Relude.Bool               (Bool (True))
import Relude.List               (NonEmpty((:|)))
import Relude.Monoid             (Endo, Semigroup ((<>)))
import Relude.String             (Text)
import Relude.Container          (One(one))
import Relude                    (Foldable (foldr), ($), (<$>), Alternative ((<|>)), Functor ((<$), fmap), ($>), (.), undefined)

import Control.Monad.Import      (MonadImport)
import Control.Monad.Chronicle   (MonadChronicle)
import Lens.Micro.Platform       (view)

import qualified Nuko.Resolver.Occourence as Occ
import qualified Nuko.Syntax.Tree         as Syntax
import qualified Data.HashMap.Strict as HashMap


-- | The main monad for the resolution. It's not necessary
-- in the type checker because the MonadState LocalNS is only
-- needed for the resolver.

type MonadResolver m =
  ( MonadImport NameSpace m
  , MonadState LocalNS m
  , MonadChronicle (Endo [ResolveError]) m
  )

makePath :: ImpPath Nm -> (Text, Range)
makePath (x :| xs) =
  foldr (\a (text, range) -> (text <> "." <> a.text, range <> a.range))
        (x.text, x.range)
        xs

resolveName :: MonadResolver m => NameKind -> Range -> Text -> m Path
resolveName kind range text = do
    let name = OccName text kind
    env <- get
    fromMaybe (terminate (CannotFindInModule (one (name.kind)) Nothing name.name range)) $
          pure (Local (ReId name.name range))                 <$  lookupEnv name env._currentNamespace._names
      <|> (\env' -> put env' $> Local (ReId name.name range)) <$> setUsage env name True
      <|> resolveAmbiguity                                    <$> lookupEnv name env._openedNames
  where
    resolveAmbiguity :: MonadResolver m => Label -> m Path
    resolveAmbiguity = \case
      (Ambiguous refs other) -> terminate (AmbiguousNames refs other)
      (Single ref mod')      -> pure (Path mod' (ReId ref range) range)

findInSpace :: MonadResolver m => NameSpace -> Range -> Text -> NonEmpty NameKind -> m OccName
findInSpace space range name occs =
    case findAltKeyValue (fmap (OccName name) occs) space._names of
      Just (res, Private) -> terminate (IsPrivate res.kind name range)
      Just (res, Public)  -> pure res
      Nothing             -> terminate (CannotFindInModule occs (Just space._modName) name range)

getModule :: MonadResolver m => Range -> Text -> m NameSpace
getModule range name = do
  module' <- gets (HashMap.lookup name . _openedModules)
  case module' of
    Just res -> pure res
    Nothing  -> terminate (CannotFindModule name range)

resolvePath :: MonadResolver m => NameKind -> XPath Nm -> m (XPath Re)
resolvePath nameKind (Syntax.Path []  (Name text range) _) = resolveName nameKind range text
resolvePath nameKind (Syntax.Path (x : xs) (Name text range) range') = do
  let (path, modRange') = makePath (x :| xs)
  module' <- getModule modRange' path
  case Occ.lookupEnv (OccName text nameKind) module'._names of
    Just _  -> pure (Path path (ReId text range) range')
    Nothing -> terminate (CannotFindInModule (one nameKind) (Just path) text range)

addGlobal :: MonadResolver m => Range -> OccName -> Visibility -> m ()
addGlobal range name vs = do
  def <- gets (view (currentNamespace . names))
  case Occ.lookupEnv name  def of
    Just _  -> terminate (AlreadyExistsName name.name name.kind range)
    Nothing -> addDef name vs
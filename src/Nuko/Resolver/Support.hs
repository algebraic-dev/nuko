module Nuko.Resolver.Support (
  Visibility(..),
  ModuleSig(..),
  ImportedWrapper(..),
  Environment,
  importModule,
  MonadResolver,
  types,
  constructors,
  fields,
  functions,
  moduleName,
  currentName,
  currentMod,
  importedModules,
  addImport,
  items,
  getPublicField,
  openFieldIn,
  getModule,
  assertAmbiguity
) where

import Control.Monad.Import    (MonadImport (importIn), ImportErrorKind (..))
import Control.Monad.Chronicle (MonadChronicle)
import Data.HashMap.Lazy       (HashMap)
import Lens.Micro.Platform     (makeLenses, over, Lens', view)
import Nuko.Utils              (terminate, flag)
import Nuko.Syntax.Range       (Range)
import Nuko.Resolver.Error     (ResolveError (..))
import Relude                  (Eq((==)), otherwise, HashSet, (.), NonEmpty ((:|)), Hashable, maybe, undefined, id, ($), (<$>))
import Relude.Monad            (MonadState, Either (..), Maybe (..), gets, modify, fromMaybe)
import Relude.String           (Text)
import Relude.Monoid           (Endo, Semigroup((<>)))
import Relude.Applicative      (pure)
import Nuko.Syntax.Tree        (Name(..))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

-- Sorry.
import Prelude (error)
import GHC.Base (Module, Applicative ((*>)))

data Visibility = Public | Private

data Ambiguity a = Ambiguous (HashSet a) | Single a
  deriving Eq

data Items a = Items
  { _types        :: HashMap Text a
  , _functions    :: HashMap Text a
  , _constructors :: HashMap Text a
  , _fields       :: HashMap Text a
  }

data ModuleSig = ModuleSig
  { _items        :: Items Visibility
  , _moduleName   :: Text
  }

type Environment = Items (Ambiguity Text)

data ImportedWrapper = ImportedWrapper
  { _currentName     :: Text
  , _currentMod      :: ModuleSig
  , _importedModules :: HashMap Text ModuleSig
  -- This module stores everything that got opened
  , _environment    :: Environment
  }

makeLenses ''ModuleSig
makeLenses ''ImportedWrapper
makeLenses ''Items

type MonadResolver m =
  ( MonadImport ModuleSig m
  , MonadState ImportedWrapper m
  , MonadChronicle (Endo [ResolveError]) m)

instance (Eq a, Hashable a) => Semigroup (Ambiguity a) where
  (<>) (Single a) (Single b)
    | a == b    = Single a
    | otherwise = Ambiguous (HashSet.fromList [a, b])
  (<>) (Ambiguous b) (Single a)    = Ambiguous (HashSet.insert a b)
  (<>) (Single a) (Ambiguous b)    = Ambiguous (HashSet.insert a b)
  (<>) (Ambiguous a) (Ambiguous b) = Ambiguous (a <> b)

-- Imports a map from the MonadImport. The getModule does not do that.
importModule :: MonadResolver m => Range -> Text -> m ModuleSig
importModule place name = do
  res <- importIn name
  case res of
    Left (CannotFind text) -> terminate (CannotFindModule text place)
    Left Cyclic            -> terminate (CyclicImport place)
    Right r'               -> pure r'

addImport :: MonadResolver m => Text -> ModuleSig -> m ()
addImport name mod = modify (over importedModules (HashMap.insert name mod))

assertAmbiguity :: MonadResolver m => Ambiguity Text -> m Text
assertAmbiguity (Single a)      = pure a
assertAmbiguity (Ambiguous set) =
  case HashSet.toList set of
    (x : xs) -> terminate (AmbiguousNames (x :| xs))
    []       -> error "You're doing some dumb shit because it should not happen!"

-- | This module tries to find the module by the alias and then after that
--   It searchs by it
getModule :: MonadResolver m => Name -> m ModuleSig
getModule mod = do
    module' <- gets (HashMap.lookup mod.text . _importedModules)
    maybe (terminate (CannotFindModule mod.text mod.range)) pure module'

getPublicField :: MonadResolver m
               => ModuleSig
               -> Name
               -> Lens' ModuleSig (HashMap Text Visibility)
               -> (Text -> Text -> Range -> ResolveError)
               -> m ()

getPublicField mod' name field failGen = do
  case HashMap.lookup name.text (view field mod') of
    Nothing      -> flag (failGen mod'._moduleName name.text name.range)
    Just Private -> flag (IsPrivate name.text name.range)
    Just Public  -> pure ()

openFieldIn :: MonadResolver m
            => ModuleSig -> Name
            -> Maybe Name
            -> (forall a. Lens' (Items a) (HashMap Text a))
            -> (Text -> Text -> Range -> ResolveError)
            -> m ()

openFieldIn mod name as field err = do
  getPublicField mod name (items . field) err
  let toName = fromMaybe name as
  modify (over (environment . field) (HashMap.insert toName.text (Single mod._moduleName)))

lookAdd :: Name -> Text -> HashMap Text (Ambiguity Text) -> HashMap Text (Ambiguity Text)
lookAdd as ty tys = HashMap.insert as.text ((maybe id (<>) (HashMap.lookup as.text tys)) (Single ty)) tys

openWithLens :: MonadResolver m => (forall a. Lens' (Items a) (HashMap Text a)) -> ModuleSig -> Name -> Name -> m (Maybe Visibility)
openWithLens lens mod name as = do
  let exists = HashMap.lookup name.text $ view lens mod._items
  case exists of
    Nothing      -> pure Nothing
    Just Private -> flag (IsPrivate name.text name.range) *> pure (Just Private)
    Just Public  -> modify (over (environment . lens) (lookAdd as name.text)) *> pure (Just Public)

openTypeOrConsAs :: MonadResolver m => ModuleSig -> Name -> Name -> m ()
openTypeOrConsAs mod name as = 
    undefined
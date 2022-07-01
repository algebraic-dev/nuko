module Nuko.Resolver.Types (
  MonadResolver,
  pathByUnqualifiedName,
  openItem
) where

import Nuko.Resolver.Occourence  (NameKind, OccName (OccName), OccEnv)
import Nuko.Resolver.Environment (LocalNS(..), NameSpace, Label (..), localNames, openedNames, joinLabels)
import Nuko.Resolver.Error       (ResolveError (AmbiguousNames))
import Nuko.Resolver.Tree        (Path (Local, Path), ReId(..))
import Nuko.Syntax.Range         (Range(..))
import Nuko.Utils                (terminate)

import Relude.Applicative        (Applicative(pure))
import Relude.Monad              (MonadState (get), Maybe, maybe, modify)
import Relude.Bool               (Bool (True), when, not)
import Relude.Functor            ((<$>))
import Relude                    (Traversable(sequence), Endo, Text, Maybe (Nothing), ($), (<|>))

import Lens.Micro.Platform       (over)
import Control.Monad.Import      (MonadImport)
import Control.Monad.Chronicle   (MonadChronicle)

import qualified Nuko.Resolver.Occourence as Occ

-- | The main monad for the resolution. It's not necessary
-- in the type checker beacause the MonadState LocalNS is only
-- needed for the resolver.

type MonadResolver m =
  ( MonadImport NameSpace m
  , MonadState LocalNS m
  , MonadChronicle (Endo [ResolveError]) m
  )

pathByUnqualifiedName :: MonadResolver m
                      => NameKind -> ReId
                      -> (Maybe Text -> Text -> Range -> ResolveError)
                      -> m Path

pathByUnqualifiedName kind id' err = do
    localNS <- get

    let localCase  = handleLocal <$> search localNS._localNames
    let openedCase = handleAmbiguity <$> search localNS._openedNames

    path <- sequence $ localCase <|> openedCase
    maybe (terminate (err Nothing id'.text id'.range)) pure path

  where
    search :: OccEnv a -> Maybe a
    search = Occ.lookupEnv (OccName id'.text kind)

    setUsed s = Occ.insertEnv (OccName id'.text kind) True s

    handleLocal :: MonadResolver m => Bool -> m Path
    handleLocal isUsed = do
      when (not isUsed) $ modify (over localNames setUsed)
      pure (Local id')

    handleAmbiguity :: MonadResolver m => Label -> m Path
    handleAmbiguity = \case
      (Single res)       -> pure (Path res id' id'.range)
      (Ambiguous others) -> terminate (AmbiguousNames others)

openItem :: MonadResolver m => OccName -> Text -> m ()
openItem item fromModule =
    modify $ over openedNames (addItem (Single fromModule))
  where
    addItem i = (Occ.updateEnvWith item (joinLabels i) i)

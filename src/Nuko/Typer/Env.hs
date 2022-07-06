module Nuko.Typer.Env (
  TypeSpace(..),
  MonadTyper,
  getKind,
) where

import Relude.Monad         (MonadIO, MonadReader (ask), Maybe (..), MonadState (get), Either (..), Monad ((>>=)), either)
import Relude.String        (Text)
import Relude.Monoid        ((<>), Endo)
import Relude.Function      ((.))

import Nuko.Utils              (terminate)
import Nuko.Typer.Types        (TKind)
import Nuko.Typer.Error        (TypeError(NameResolution))
import Nuko.Resolver.Tree      (Path (..), ReId(text))
import Data.HashMap.Strict     (HashMap, lookup)
import Relude.Applicative      (Applicative(pure))
import Control.Monad.Chronicle (MonadChronicle)

data TypeSpace = TypeSpace
  { _tsTypes :: HashMap Text TKind
  }

type MonadTyper m =
  ( MonadIO m
  , MonadReader TypeSpace m
  , MonadState TypeSpace m
  , MonadChronicle (Endo [TypeError]) m
  )

getTypeSpaceKind ::  Text -> TypeSpace -> Either TypeError TKind
getTypeSpaceKind name' ts =
  let res  = lookup name' (_tsTypes ts)
  in case res of
      Just res' -> pure res'
      Nothing   -> Left (NameResolution name')

getKind :: MonadTyper m => Path -> m TKind
getKind (Local name') = get >>= either terminate pure . getTypeSpaceKind name'.text
getKind (Path mod name' _) = ask >>= either terminate pure . getTypeSpaceKind (mod <> "." <> name'.text)
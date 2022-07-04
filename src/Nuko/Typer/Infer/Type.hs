module Nuko.Typer.Infer.Type (
  inferTy
) where

import Nuko.Tree             (Ty(TForall, TId, TArrow), Re)
import Nuko.Typer.Env        (Name, MonadTyper)
import Data.Maybe            (fromJust)

import Relude.Applicative    ((<*>), Applicative (pure))
import Relude.Container      (HashMap)
import Relude.Functor        ((<$>))
import Relude.Monad          (asks, MonadReader (local), ReaderT (runReaderT))
import Relude.Numeric        (Int)
import Relude.String         (Text)
import Relude                (error, (.))

import Nuko.Resolver.Tree    (Path(Local), ReId(text))
import Nuko.Typer.Types      (TTy (..), Virtual, Normal, evaluate)

import qualified Data.HashMap.Strict     as HashMap
import qualified Data.IntMap             as IntMap

inferTy :: MonadTyper m => Ty Re -> m (TTy Virtual)
inferTy ty = do
    result <- runReaderT (inferTy' ty) (HashMap.empty)
    pure (evaluate IntMap.empty result)
  where
    addName :: Text -> HashMap Text Int -> HashMap Text Int
    addName name map = HashMap.insert name (HashMap.size map) map

    inferTy' :: MonadTyper m => Ty Re -> ReaderT (HashMap Name Int) m (TTy Normal)
    inferTy' = \case
        TId (Local x) _    -> (TyVar . fromJust) <$> asks (HashMap.lookup x.text)
        TArrow a b  _      -> TyFun <$> inferTy' a <*> inferTy' b
        TForall name ty' _ -> local (addName name.text) (TyForall name.text <$> inferTy' ty')
        _                  -> error "Not implemented Yettt!"
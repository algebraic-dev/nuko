module Nuko.Typer.Types (
  TTy(..),
  Hole(..),
  TKind(..),
  KiScheme(..),
  TyHole,
  Name,
  Lvl,
  Virtual,
  Normal,
  If,
  evaluate,
  removeHole,
  printTy
) where

import Relude.String       (Text, ToString(toString))
import Relude.Container    (IntMap)
import Relude.Applicative  (Applicative(pure))
import Relude.Bool         (Bool(..))
import Relude.Monad        (MonadIO(..), fromMaybe)
import Relude.Monoid       (Semigroup((<>)))
import Relude.Numeric      (Int)
import Relude              (error, ($), show, (.))

import GHC.IO              (unsafePerformIO)
import Data.IORef          (IORef, readIORef, writeIORef)
import Text.Show           (Show (show))

import qualified Data.IntMap.Strict  as IntMap

type Lvl = Int

type Name = Text

type TyHole a = IORef (Hole (TTy a))

type KiHole = IORef (Hole TKind)

data Hole ty
  = Empty Name Lvl
  | Filled ty

-- Useful type aliases to add some context to these things

type Virtual = 'True
type Normal  = 'False

data KiScheme = KiScheme Int TKind

data TKind where
  KiStar  :: TKind
  KiVar   :: Int    -> TKind
  KiFun   :: TKind  -> TKind -> TKind
  KiHole  :: KiHole -> TKind

type family If (v :: Bool) a b where
  If 'True  a _ = a
  If 'False _ b = b

data TTy (v :: Bool) where
  TyForall  :: Name -> (If a (TTy a -> TTy a) (TTy a)) -> TTy a
  TyHole    :: TyHole 'True                            -> TTy a
  TyVar     :: Lvl                                     -> TTy a
  TyIdent   :: Text -> Text                            -> TTy a
  TyFun     :: TTy a -> TTy a                          -> TTy a

printTy :: TTy Virtual -> Text
printTy =
    helper IntMap.empty
  where
    helper :: IntMap Name -> TTy Virtual -> Text
    helper ctx = \case
      TyIdent a b -> a <> "." <> b
      TyVar lvl -> fromMaybe ("[No " <> Relude.show lvl <> "]") (IntMap.lookup lvl ctx)
      TyFun a b -> "(" <> (helper ctx a) <> " -> " <> (helper ctx b) <> ")"
      TyForall n f ->
        let res = helper (IntMap.insert (IntMap.size ctx) n ctx) (f (TyVar (IntMap.size ctx))) in
        "(forall " <> n <> ". " <> res <> ")"
      TyHole hole -> unsafePerformIO $ do
        content <- readIORef hole
        case content of
          Empty n r -> pure ("^" <> n <> "." <> Relude.show r)
          Filled a  -> pure ("~" <> helper ctx a)

instance Show (TTy Virtual) where
  show = toString . printTy

evaluate :: IntMap (TTy Virtual) -> TTy Normal -> TTy Virtual
evaluate env = \case
  TyIdent a b      -> TyIdent a b
  TyHole hole      -> TyHole hole
  TyVar x          -> fromMaybe (error "Boop") (IntMap.lookup x env)
  TyForall name ty -> TyForall name (\x -> evaluate (IntMap.insert (IntMap.size env) x env) ty)
  TyFun from to    -> TyFun (evaluate env from) (evaluate env to)

removeHole :: MonadIO m => TTy Virtual -> m (TTy Virtual)
removeHole = \case
    TyHole hole -> smash hole
    a           -> pure a
  where
    smash :: MonadIO m => TyHole Virtual -> m (TTy Virtual)
    smash hole = do
      inside <- liftIO (readIORef hole)
      case inside of
        Filled (TyHole hole') -> do
          result <- smash hole'
          liftIO (writeIORef hole (Filled result))
          pure result
        Filled a -> pure a
        _        -> pure (TyHole hole)
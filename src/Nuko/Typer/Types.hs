module Nuko.Typer.Types (
  Hole(..),
  TKind(..),
  Virtual,
  TTy(..),
  KiHole,
  Normal,
  TyHole,
  PType,
  Int,
  If,
  convertType,
  printTy,
  printKind,
) where

import Relude.Container    (IntMap)
import Relude.Applicative  (Applicative(pure))
import Relude.Lifted       (IORef, readIORef)
import Relude.Bool         (Bool(..))
import Relude.Monad        (fromMaybe, MonadIO)
import Relude.Monoid       (Semigroup((<>)))
import Relude.Numeric      (Int)
import Relude              (show, Text, MonadIO (liftIO))
import Unsafe.Coerce       (unsafeCoerce)

import Nuko.Resolver.Tree  (Path (..), ReId(..))

import qualified Data.IntMap.Strict as IntMap
import Pretty.Tree (PrettyTree (..), Tree (..))
import GHC.IO (unsafePerformIO)

type TyHole = IORef (Hole (TTy Virtual))
type KiHole = IORef (Hole TKind)

type family If (v :: Bool) a b where
  If 'True  a _ = a
  If 'False _ b = b

data Hole ty where
  Empty       :: Text -> Int        -> Hole ty
  Filled      :: ty                 -> Hole ty

-- Useful type aliases to add some context to these things

type Virtual = 'True

type Normal  = 'False

type PType = (TTy Virtual, TKind)

data TKind where
  KiStar  :: TKind
  KiFun   :: TKind -> TKind -> TKind
  KiHole  :: KiHole -> TKind

data TTy (v :: Bool) where
  TyForall  :: Text -> (If a (TTy a -> TTy a) (TTy a)) -> TTy a
  TyHole    :: TyHole                                  -> TTy a
  TyVar     :: Int                                     -> TTy a
  TyIdent   :: Path                                    -> TTy a
  TyFun     :: TTy a -> TTy a                          -> TTy a
  TyApp     :: TTy a -> TTy a                          -> TTy a

instance PrettyTree (TTy Virtual) where
  prettyTree ty = Node "Type" [unsafePerformIO (printTy ty)] []

instance PrettyTree (TKind) where
  prettyTree ty = Node "Kind" [unsafePerformIO (printKind ty)] []

convertType :: TKind -> TKind
convertType t = unsafeCoerce t

printTy :: MonadIO m => TTy Virtual -> m Text
printTy =
    helper IntMap.empty
  where
    helper :: MonadIO m => IntMap Text -> TTy Virtual -> m Text
    helper ctx = \case
      TyIdent (Local re)      -> pure re.text
      TyIdent (Path mod re _) -> pure (mod <> "." <> re.text)
      TyVar int   -> pure (fromMaybe ("[No " <> Relude.show int <> "]") (IntMap.lookup int ctx))
      TyFun a b   -> do
        resA <- helper ctx a
        resB <- helper ctx b
        pure ("(" <> resA <> " -> " <> resB <> ")")
      TyForall n f -> do
        res <- helper (IntMap.insert (IntMap.size ctx) n ctx) (f (TyVar (IntMap.size ctx)))
        pure ("(forall " <> n <> ". " <> res <> ")")
      TyHole hole -> do
        content <- liftIO (readIORef hole)
        case content of
          Empty n r     -> pure ("^" <> n <> "." <> Relude.show r)
          Filled a      -> do
            filledRes <- helper ctx a
            pure ("~" <> filledRes)
      TyApp a b -> do
        resA <- helper ctx a
        resB <- helper ctx b
        pure ("(" <> resA <> " " <> resB <> ")")

printKind :: MonadIO m => TKind -> m Text
printKind = \case
  KiStar    -> pure "*"
  KiFun f t -> do
    resF <- printKind f
    resT <- printKind t
    pure ("(" <> resF <> " -> " <> resT <> ")")
  KiHole hole -> do
    content <- readIORef hole
    case content of
      Empty n r     ->
        pure ("^" <> n <> "." <> Relude.show r)
      Filled a      -> do
        resA <- printKind a
        pure ("~" <> resA)
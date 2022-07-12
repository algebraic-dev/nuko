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
  implTy,
  generalizeOver,
  printTy,
  printKind,
  dereferenceKind,
  removeKindHoles,
  dereferenceType
) where

import Relude.Applicative  (Applicative(pure, (<*>)))
import Relude.Lifted       (IORef, readIORef, writeIORef)
import Relude.Bool         (Bool(..))
import Relude.Monad        (fromMaybe, MonadIO)
import Relude.Monoid       (Semigroup((<>)))
import Relude.Numeric      (Int)
import Relude.Functor      ((<$>))
import Relude              (show, Text, MonadIO (liftIO), Applicative ((*>)), reverse, Num ((+)))

import Nuko.Resolver.Tree  (Path (..), ReId(..))
import GHC.IO              (unsafePerformIO)
import Pretty.Tree         (PrettyTree (..), Tree (..))

import Data.List ((!!))
import Relude.List ((!!?))
import Nuko.Report.Range (emptyRange)

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
  TyApp     :: TKind -> TTy a -> TTy a                 -> TTy a

instance PrettyTree (TTy Virtual) where
  prettyTree ty = Node "Type" [unsafePerformIO (printTy [] ty)] []

instance PrettyTree (TKind) where
  prettyTree ty = Node "Kind" [unsafePerformIO (printKind  ty)] []

generalizeOver :: [Text] -> TTy Virtual -> TTy Virtual
generalizeOver names tty =
    go names []
  where
    subs :: Int -> [TTy Virtual] -> TTy Virtual -> TTy Virtual
    subs scope types = \case
      TyVar n -> types !! n
      TyForall t f -> TyForall t (\n -> subs (scope + 1) (n : types) (f n))
      TyHole h     ->
        case unsafePerformIO (readIORef h) of
          Empty {} -> TyHole h
          Filled f -> subs scope types f
      TyFun f t   -> TyFun (subs scope types f) (subs scope types t)
      TyApp k f t -> TyApp k (subs scope types f) (subs scope types t)
      TyIdent t   -> TyIdent t
    go []       tys = subs 0 (reverse tys) tty
    go (x : xs) tys = TyForall x (\f -> go xs (f : tys))

implTy :: TTy Virtual -> TTy Virtual -> TTy Virtual
implTy to = \case
  TyVar 0      -> to
  TyForall t f -> TyForall t f
  TyHole h     ->
    case unsafePerformIO (readIORef h) of
      Empty {} -> TyHole h
      Filled f -> implTy to f
  TyVar n -> TyVar n
  TyFun f t   -> TyFun (implTy to f) (implTy to t)
  TyApp k f t -> TyApp k (implTy to f) (implTy to t)
  TyIdent t   -> TyIdent t

dereferenceKind :: MonadIO m => TKind -> m TKind
dereferenceKind = \case
  KiHole hole -> do
    result <- readIORef hole
    case result of
      Empty {} -> pure (KiHole hole)
      Filled t -> dereferenceKind t
  KiFun a b -> KiFun <$> dereferenceKind a <*> dereferenceKind b
  other -> pure other

dereferenceType :: MonadIO m => TTy Virtual -> m (TTy Virtual )
dereferenceType = \case
  TyHole hole -> do
    result <- readIORef hole
    case result of
      Empty {} -> pure (TyHole hole)
      Filled t -> dereferenceType t
  other -> pure other

removeKindHoles :: MonadIO m => TKind -> m TKind
removeKindHoles = \case
  KiHole hole -> do
    result <- readIORef hole
    case result of
      Empty {} -> writeIORef hole (Filled KiStar) *> pure KiStar
      Filled t -> dereferenceKind t
  KiFun a b -> KiFun <$> removeKindHoles a <*> removeKindHoles b
  other -> pure other

printTy :: MonadIO m => [Text] -> TTy Virtual -> m Text
printTy env =
    helper env
  where
    helper :: MonadIO m => [Text] -> TTy Virtual -> m Text
    helper ctx = \case
      TyIdent (Local re)      -> pure re.text
      TyIdent (Path mod re _) -> pure (mod <> "." <> re.text)
      TyVar int   -> pure (fromMaybe ("[No " <> Relude.show int <> "]") (ctx !!? int))
      TyFun a b   -> do
        resA <- helper ctx a
        resB <- helper ctx b
        pure ("(" <> resA <> " -> " <> resB <> ")")
      TyForall n f -> do
        res <- helper (n : ctx) (f (TyIdent (Local (ReId n emptyRange))))
        pure ("(forall " <> n <> ". " <> res <> ")")
      TyHole hole -> do
        content <- liftIO (readIORef hole)
        case content of
          Empty n r     -> pure ("^" <> n <> "." <> Relude.show r)
          Filled a      -> do
            filledRes <- helper ctx a
            pure ("~" <> filledRes)
      TyApp _ a b -> do
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
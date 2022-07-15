module Nuko.Typer.Types (
  Hole(..),
  TKind(..),
  TTy(..),
  Relation(..),
  derefKind,
  derefTy,
  evaluate,
  quote,
  generalizeWith,
  printTy,
  printRealTy
) where

import Relude             ((.), reverse, Num ((-), (+)), Text, Semigroup ((<>)))
import Relude.Lifted      (IORef, readIORef)
import Relude.Numeric     (Int)
import Relude.Unsafe      ((!!))

import Nuko.Names         (Path, Name (..), TyName)
import GHC.IO             (unsafePerformIO)
import Pretty.Format      (Format(..))
import Pretty.Tree        (PrettyTree(prettyTree), Tree (..))

data Relation = Real | Virtual

type TyHole = IORef (Hole (TTy 'Virtual))
type KiHole = IORef (Hole TKind)

data Hole ty where
  Empty  :: Name TyName -> Int -> Hole ty
  Filled :: ty -> Hole ty

data TKind where
  KiStar :: TKind
  KiFun  :: TKind -> TKind -> TKind
  KiHole :: KiHole -> TKind

type family If (v :: Relation) a b where
  If 'Virtual  a _ = a
  If 'Real _ b = b

data TTy (v :: Relation) where
  TyForall  :: Name TyName -> (If a (TTy a -> TTy a) (TTy a)) -> TTy a
  TyHole    :: TyHole -> TTy a
  TyVar     :: Int -> TTy a
  TyIdent   :: Path (Name TyName) -> TTy a
  TyFun     :: TTy a -> TTy a -> TTy a
  TyApp     :: TKind -> TTy a -> TTy a -> TTy a

derefKind :: TKind -> TKind
derefKind = \case
  KiHole hole -> do
    case unsafePerformIO (readIORef hole) of
      Empty {} -> KiHole hole
      Filled  r -> derefKind r
  KiFun a b -> KiFun (derefKind a) (derefKind b)
  other -> other

derefTy :: TTy 'Virtual -> TTy 'Virtual
derefTy = \case
  TyHole hole -> do
    case unsafePerformIO (readIORef hole) of
      Empty {} -> TyHole hole
      Filled  r -> derefTy r
  TyForall ident fn -> TyForall ident (derefTy . fn)
  TyFun t f -> TyFun (derefTy t) (derefTy f)
  TyApp k t f -> TyApp (derefKind k) (derefTy t) (derefTy f)
  other -> other

-- Quoting and evaluation

evaluate :: [TTy 'Virtual] -> TTy 'Real -> TTy 'Virtual
evaluate types = \case
  TyHole hole -> TyHole hole
  TyForall ident fn -> TyForall ident (\f -> evaluate (f : types) fn)
  TyFun t f -> TyFun (evaluate types t) (evaluate types f)
  TyApp k t f -> TyApp k (evaluate types t) (evaluate types f)
  TyIdent t -> TyIdent t
  TyVar i -> types !! i

quote :: Int -> TTy 'Virtual -> TTy 'Real
quote lvl = \case
  TyHole hole -> TyHole hole
  TyForall ident fn -> TyForall ident (quote (lvl + 1) (fn (TyVar lvl)))
  TyFun t f -> TyFun (quote lvl t) (quote lvl f)
  TyApp k t f -> TyApp k (quote lvl t) (quote lvl f)
  TyIdent t -> TyIdent t
  TyVar i -> TyVar (lvl - i - 1)

generalizeWith :: [Name TyName] -> TTy 'Real -> TTy 'Virtual
generalizeWith [] ty = evaluate [] ty
generalizeWith xs ty =
    go xs []
  where
    go [] tys        = evaluate (reverse tys) ty
    go (x : xs') tys = TyForall x (\f -> go xs' (f : tys))

printRealTy :: TTy 'Real -> Text
printRealTy = \case
  TyHole hole ->
    case unsafePerformIO (readIORef hole) of
      Empty n i -> "[" <> format n <> "." <> format i <> "]"
      Filled f  -> printTy f

  TyFun a@(TyForall {}) b -> "(" <> printRealTy a <> ") -> " <> printRealTy b
  TyFun a@(TyFun {}) b -> "(" <> printRealTy a <> ") -> " <> printRealTy b

  TyApp _ a@(TyForall {}) b -> "(" <> printRealTy a <> ") " <> printRealTy b
  TyApp _ a@(TyFun {}) b -> "(" <> printRealTy a <> ") " <> printRealTy b
  TyApp _ a b@(TyFun {}) -> printRealTy a <> " (" <> printRealTy b <> ")"
  TyApp _ a b@(TyApp {}) -> printRealTy a <> " (" <> printRealTy b <> ")"

  TyForall ident fn -> "forall " <> format ident <> "." <> printRealTy fn
  TyFun t f   -> printRealTy t <> " -> " <> printRealTy f
  TyApp _ t f -> printRealTy t <> " " <> printRealTy f
  TyIdent t   -> format t
  TyVar i     -> "^" <> format i

printTy :: TTy 'Virtual -> Text
printTy = printRealTy . quote 0 . derefTy

printKind :: TKind -> Text
printKind =
    go . derefKind
  where
    go = \case
      KiHole hole -> do
        case unsafePerformIO (readIORef hole) of
          Empty {} -> "?"
          Filled r -> printKind r
      KiFun t@(KiFun {}) f ->  "(" <> printKind t <> ") -> " <> printKind f
      KiFun t f ->  printKind t <> " -> " <> printKind f
      KiStar -> "*"

instance Format TKind where format = printKind
instance Format (TTy 'Virtual) where format = printTy
instance Format (TTy 'Real) where format = printRealTy

instance PrettyTree (TTy 'Virtual) where
  prettyTree a = Node ("Ty") [format a] []

instance PrettyTree (TKind) where
  prettyTree a = Node ("Kind") [format a] []
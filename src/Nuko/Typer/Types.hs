module Nuko.Typer.Types (
  TyHole,
  Hole(..),
  TKind(..),
  TTy(..),
  Relation(..),
  derefTy,
  evaluate,
  quote,
  generalizeWith,
  printTy,
  generalizeNames,
  printRealTy,
) where

import Relude             ((.), reverse, Num ((-), (+)), Text, Semigroup ((<>)), ($), Foldable (length, foldr), Ord ((>=), (<)), (||), (<$>), unwords)
import Relude.Lifted      (IORef, readIORef)
import Relude.Numeric     (Int)
import Relude.Unsafe      ((!!))

import Nuko.Typer.Kinds   (Hole (..), TKind (..), derefKind)
import Nuko.Names         (Name (..), TyName, Qualified (..))
import GHC.IO             (unsafePerformIO)
import Pretty.Format      (Format(..))
import Pretty.Tree        (PrettyTree(prettyTree), Tree (..))

data Relation = Real | Virtual

type TyHole = IORef (Hole (TTy 'Virtual))

type family If (v :: Relation) a b where
  If 'Virtual  a _ = a
  If 'Real _ b = b

data TTy (v :: Relation) where
  TyForall  :: Name TyName -> (If a (TTy a -> TTy a) (TTy a)) -> TTy a
  TyHole    :: TyHole -> TTy a
  TyVar     :: Int -> TTy a
  TyIdent   :: Qualified (Name TyName) -> TTy a
  TyFun     :: TTy a -> TTy a -> TTy a
  TyApp     :: TKind -> TTy a -> TTy a -> TTy a
  TyErr     :: TTy a

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
  TyErr -> TyErr
  TyHole hole -> TyHole hole
  TyForall ident fn -> TyForall ident (\f -> evaluate (f : types) fn)
  TyFun t f -> TyFun (evaluate types t) (evaluate types f)
  TyApp k t f -> TyApp k (evaluate types t) (evaluate types f)
  TyIdent t -> TyIdent t
  TyVar i ->
    if i >= length types
      then TyVar i
      else types !! i

quote :: Int -> TTy 'Virtual -> TTy 'Real
quote lvl = \case
  TyErr -> TyErr
  TyHole hole -> TyHole hole
  TyForall ident fn -> TyForall ident (quote (lvl + 1) (fn (TyVar lvl)))
  TyFun t f -> TyFun (quote lvl t) (quote lvl f)
  TyApp k t f -> TyApp k (quote lvl t) (quote lvl f)
  TyIdent t -> TyIdent t
  TyVar i -> TyVar (lvl - i - 1)

generalizeWith :: [Name TyName] -> TTy 'Real -> (TTy 'Virtual -> TTy 'Virtual) -> TTy 'Virtual
generalizeWith [] ty _ = evaluate [] ty
generalizeWith xs ty fun =
    go xs []
  where
    go [] tys        = fun $ evaluate (reverse tys) ty
    go (x : xs') tys = TyForall x (\f -> go xs' (f : tys))

generalizeNames :: [Name TyName] -> TTy 'Real -> TTy 'Real
generalizeNames xs ty = foldr TyForall ty xs

printRealTy :: TTy 'Real -> Text
printRealTy =
    go []
  where
    getSeq :: TTy 'Real -> ([Name TyName], TTy 'Real)
    getSeq (TyForall x t) = let (n, f) = getSeq t in (x : n, f)
    getSeq other = ([], other)

    go :: [Name TyName] -> TTy 'Real -> Text
    go env = \case
      TyErr -> "ERR"
      TyHole hole ->
        case unsafePerformIO (readIORef hole) of
          Empty n i -> "[" <> format n <> "." <> format i <> "]"
          Filled f  -> printTy f

      TyFun a@(TyForall {}) b -> "(" <> go env a <> ") -> " <> go env b
      TyFun a@(TyFun {}) b -> "(" <> go env a <> ") -> " <> go env b

      TyApp _ a@(TyForall {}) b -> "(" <> go env a <> ") " <> go env b
      TyApp _ a@(TyFun {}) b -> "(" <> go env a <> ") " <> go env b
      TyApp _ a b@(TyFun {}) -> go env a <> " (" <> go env b <> ")"
      TyApp _ a b@(TyApp {}) -> go env a <> " (" <> go env b <> ")"

      TyForall ident fn ->
        let (ident', fn') = getSeq (TyForall ident fn) in
        "forall " <> unwords (format <$> ident') <> ". " <> go (reverse ident' <> env) fn'
      TyFun t f   -> go env t <> " -> " <> go env f
      TyApp _ t f -> go env t <> " " <> go env f
      TyIdent t   -> format t.qInfo
      TyVar i     ->
        if i >= length env || i < 0
          then "^" <> format i
          else format (env !! i)

printTy :: TTy 'Virtual -> Text
printTy = printRealTy . quote 0 . derefTy

instance Format (TTy 'Virtual) where format = printTy
instance Format (TTy 'Real) where format = printRealTy

instance PrettyTree (TTy 'Virtual) where
  prettyTree a = Node "Ty" [format a] []

instance PrettyTree (TTy 'Real) where
  prettyTree a = Node "Ty" [format a] []
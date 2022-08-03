module Nuko.Typer.Kinds (
  KiHole,
  Hole(..),
  TKind(..),
  derefKind,
  printKind
) where

import Relude

import Nuko.Names    (Name (..), TyName)
import Pretty.Format (Format (..))
import Pretty.Tree   (PrettyTree (prettyTree), Tree (..))

import GHC.IO        (unsafePerformIO)

type KiHole = IORef (Hole TKind)

data Hole ty where
  Empty  :: Name TyName -> Int -> Hole ty
  Filled :: ty -> Hole ty

data TKind where
  KiStar :: TKind
  KiFun  :: TKind -> TKind -> TKind
  KiHole :: KiHole -> TKind

derefKind :: TKind -> TKind
derefKind = \case
  KiHole hole -> do
    case unsafePerformIO (readIORef hole) of
      Empty {}  -> KiHole hole
      Filled  r -> derefKind r
  KiFun a b -> KiFun (derefKind a) (derefKind b)
  other -> other

-- Pretty printing

printKind :: TKind -> Text
printKind =
    go . derefKind
  where
    go = \case
      KiHole hole -> do
        case unsafePerformIO (readIORef hole) of
          Empty {} -> "_"
          Filled r -> printKind r
      KiFun t@(KiFun {}) f ->  "(" <> printKind t <> ") -> " <> printKind f
      KiFun t f ->  printKind t <> " -> " <> printKind f
      KiStar -> "*"

instance Format TKind where format = printKind

instance PrettyTree TKind where
  prettyTree a = Node "Kind" [format a] []

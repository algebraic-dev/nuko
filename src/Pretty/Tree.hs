module Pretty.Tree (
  PrettyTree(..),
  Tree(..),
  inlineTree,
) where

import GHC.Generics     (V1, U1, K1 (K1), type (:+:)(..), type (:*:)(..), Generic (from, Rep), D1, M1 (M1), Constructor (conName), C1, S1, Selector (selName))
import Data.Text        (length, intercalate, Text)

import Relude           ((.), show, ToText (toText), ($), unwords,Ord ((<)), NonEmpty, Int, Functor (fmap), Foldable (toList), Void, otherwise)
import Relude.Monoid    (Semigroup ((<>)), Monoid (mconcat))
import Relude.Monad     (Maybe(..), Either(..), maybe, Either (Right))
import Relude.Bool      (Bool(..))
import Relude.Container (HashMap, HashSet)
import Relude.Foldable  (sum)
import Relude.List      (map)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet        as HashSet

data Tree = Node Text [Text] [Tree]

inlineTree :: Tree -> Tree
inlineTree tree@(Node a [] c) =
    maybe tree (\f -> if sum (map length f) < 60 then Node a f [] else tree) (canInline [] c)
  where
    canInline :: [Text] -> [Tree] -> Maybe [Text]
    canInline f (Node a' [] [] : xs) = canInline (a' : f) xs
    canInline f (Node a' c' [] : xs) = canInline (("(" <> unwords (a' : c') <> ")") : f) xs
    canInline f []                   = Just f
    canInline _ _                    = Nothing

inlineTree tree = tree

class GPrettyTree f where
  genPrettyTree :: f a -> Tree

instance GPrettyTree V1 where
  genPrettyTree _ = Node "Empty" [] []

instance GPrettyTree U1 where
  genPrettyTree _ = Node "Unit" [] []

instance PrettyTree c => GPrettyTree (K1 i c) where
  genPrettyTree (K1 a) = prettyTree a

instance (GPrettyTree a, GPrettyTree b) => GPrettyTree (a :+: b) where
  genPrettyTree (L1 t) = genPrettyTree t
  genPrettyTree (R1 t) = genPrettyTree t

instance (GPrettyTree a, GPrettyTree b) => GPrettyTree (a :*: b) where
  genPrettyTree (a' :*: b') =
    case (genPrettyTree a', genPrettyTree b') of
      (Node "-Prod" f a, Node "-Prod" g b) -> Node "-Prod" (f <> g) (a <> b)
      (Node "-Prod" f a,       Node b g c) -> Node "-Prod" f (a <> [Node b g c])
      (Node b f c,       Node "-Prod" g d) -> Node "-Prod" g (Node b f c : d)
      (Node a f c,             Node b g d) -> Node "-Prod" [] [Node a f c, Node b g d]

instance (Constructor c, GPrettyTree f) => GPrettyTree (C1 c f) where
  genPrettyTree x@(M1 a) =
      inlineTree $
        case genPrettyTree a of
          Node "-Prod" f r  -> Node (toText $ conName x) f r
          Node "Unit" [] [] -> Node (toText $ conName x) [] []
          other             -> Node (toText $ conName x) [] [other]

instance GPrettyTree g => GPrettyTree (D1 f g) where
  genPrettyTree (M1 a) = genPrettyTree a

instance (Selector f, GPrettyTree g) => GPrettyTree (S1 f g) where
  genPrettyTree x@(M1 a) =
    case (toText $ selName x <> ":", genPrettyTree a) of
      (":", n)              -> n
      (name, Node a' [] []) -> Node name [a'] []
      (name, Node a' other [])
        | sum (map length other) < 30 -> Node name ["(" <> unwords (a' : other) <> ")"] []
        | otherwise                   -> Node name [] [Node a' other []]
      (name, other) -> Node name [] [other]

class PrettyTree a where
  prettyTree :: a -> Tree

  default prettyTree :: (Generic a, GPrettyTree (Rep a)) => a -> Tree
  prettyTree = genPrettyTree . from

  prettyShowTree :: a -> Text
  prettyShowTree = prettyShow' True "" . prettyTree

prettyShow' :: Bool -> Text -> Tree -> Text
prettyShow' last ident (Node text f others) =
    ident <> (if last then " └" else " ├") <> unwords (text : f) <> "\n" <> mconcat (mapOn others)
  where
    mapOn []       = []
    mapOn [x]      = [prettyShow' True (ident <> (if last then "  " else " |")) x]
    mapOn (x : xs) = prettyShow' False (ident <> (if last then "  " else " |")) x : mapOn xs

instance PrettyTree Int where
  prettyTree a = Node (show a) [] []

instance PrettyTree Text where
  prettyTree a = Node a [] []

instance (PrettyTree a, PrettyTree b) => PrettyTree (a, b) where
  prettyTree (a, b) = Node "Prod" [] [prettyTree a, prettyTree b]

instance (PrettyTree a, PrettyTree b) => PrettyTree (Either a b) where
  prettyTree (Left a) = Node "Left" [] [prettyTree a]
  prettyTree (Right a) = Node "Right" [] [prettyTree a]

instance (PrettyTree a, PrettyTree b) => PrettyTree (HashMap a b) where
  prettyTree a = Node "HashMap" [] (map prettyTree $ HashMap.toList a)

instance PrettyTree a => PrettyTree (HashSet a) where
  prettyTree a = Node "HashSet" [] (map prettyTree $ HashSet.toList a)

instance PrettyTree Bool where
  prettyTree a = Node (show a) [] []

instance PrettyTree a => PrettyTree [a] where
  prettyTree [] = Node "Empty List" [] []
  prettyTree a  = inlineTree $ Node "List" [] (map prettyTree a)

instance PrettyTree a => PrettyTree (NonEmpty a) where
  prettyTree a = inlineTree $ Node "NonEmpty" [] (toList $ fmap prettyTree a)

instance PrettyTree a => PrettyTree (Maybe a) where
  prettyTree (Just a) = inlineTree $ Node "Just" [] [prettyTree a]
  prettyTree Nothing  = Node "Nothing" [] []

instance PrettyTree Void where
  prettyTree a = Node (show a) [] []
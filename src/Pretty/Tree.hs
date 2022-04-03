module Pretty.Tree (drawTree, Node(..), SimpleTree(..)) where

import Data.Text (Text, unpack)

data Node = Node String [Node]

lastChar :: Bool -> String -> String
lastChar last' ident = ident ++ (if last' then "└" else "├")

drawTree' :: String -> Bool -> Node -> String
drawTree' ident last' (Node name []) = lastChar last' ident ++ name ++ "\n"
drawTree' ident last' (Node name nodes) =
  concat [lastChar last' ident, name, "\n", concat (mapLine nodes)]
  where
    newIdent = ident ++ (if not last' then "│" else "  ")
    mapLine [] = []
    mapLine [x] = [drawTree' newIdent True x]
    mapLine (x : xs) = drawTree' newIdent False x : mapLine xs

drawTree :: SimpleTree a => a -> String
drawTree = drawTree' "" True . toTree

class SimpleTree a where
  toTree :: a -> Node

instance SimpleTree a => SimpleTree [a] where
  toTree = Node "List" . map toTree

instance (SimpleTree a, SimpleTree b) => SimpleTree (a, b) where
  toTree (a, b) = Node "L-Tuple" [toTree a, toTree b]

instance SimpleTree Text where
  toTree t = Node (unpack t) []

instance (SimpleTree a, SimpleTree b) => SimpleTree (Either a b) where
  toTree (Left a) = Node "Either-Left" [toTree a]
  toTree (Right a) = Node "Either-Right" [toTree a]

instance SimpleTree a => SimpleTree (Maybe a) where
  toTree (Just a) = Node "Just" [toTree a]
  toTree Nothing = Node "Nothing" []
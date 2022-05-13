{-| Base module for every Syntax tree in "Trees that grow" format that
    will be used in the project. Probably this project will have only
    a few types of abstract trees
    - Normal: Only positions are stored inside the tree
    - Typed: Positions and types together
    - Lowered: No types or patterns are included here, it's an untyped ast
    - Optimized: No lambdas or things like that, it will have a few structures
-}
module Nuko.Tree where

import Nuko.Tree.Expr
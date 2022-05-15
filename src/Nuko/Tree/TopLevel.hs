module Nuko.Tree.TopLevel (
  LetDecl(..),
  TypeDecl(..),
  Program(..),
  TypeDeclArg(..),
  XLetDecl,
  XTypeDecl,
  XTypeProd,
  XTypeSym,
  XTypeSum,
  XProgram,
) where

import Nuko.Tree.Expr ( Expr, Pat, Name, Type )
import Data.List.NonEmpty (NonEmpty)

data LetDecl x = LetDecl
  { declName :: Name x
  , declArgs :: [Pat x]
  , declBody :: Expr x
  , declExt  :: XLetDecl x
  }

data TypeDeclArg x
  = TypeSym (Type x)
  | TypeProd [(Name x, Type x)]
  | TypeSum (NonEmpty (Name x, [Type x]))

data TypeDecl x = TypeDecl
  { tyName :: Name x
  , tyArgs :: [Name x]
  , tyDecl :: TypeDeclArg x
  }

data Program x = Program
  { tyDecls    :: [TypeDecl x]
  , letDecls   :: [LetDecl x]
  , programExt :: XProgram x
  }

type family XLetDecl x
type family XProgram x
type family XTypeDecl x

type family XTypeSym x
type family XTypeProd x
type family XTypeSum x

module Nuko.Tree.TopLevel (
  LetDecl(..),
  TypeDecl(..),
  Program(..),
  TypeDeclArg(..),
  Import(..),
  XLetDecl,
  XTypeDecl,
  XTypeProd,
  XTypeSym,
  XTypeSum,
  XProgram,
  XImport
) where

import Nuko.Tree.Expr ( Expr, Name, Type, Path )
import Data.List.NonEmpty (NonEmpty)

data LetDecl x = LetDecl
  { declName :: Name x
  , declArgs :: [(Name x, Type x)]
  , declBody :: Expr x
  , declRet  :: Maybe (Type x)
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

data Import x  = Import
  { modName :: Path x
  , asName  :: Maybe (Name x)
  , impExt  :: XImport x
  }

data Program x = Program
  { typeDecls  :: [TypeDecl x]
  , letDecls   :: [LetDecl x]
  , impDecls   :: [Import x]
  , openDecls  :: [Path x]
  , programExt :: XProgram x
  }

type family XLetDecl x
type family XProgram x
type family XTypeDecl x
type family XImport x

type family XTypeSym x
type family XTypeProd x
type family XTypeSum x

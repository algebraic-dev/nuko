module Nuko.Tree.TopLevel (
  LetDecl(..),
  TypeDecl(..),
  Program(..),
  TypeDeclArg(..),
  Import(..),
  ImportModifier(..),
  ImportDeps(..),
  ImportDepsKind(..),
  XLetDecl,
  XTypeDecl,
  XTypeProd,
  XTypeSym,
  XTypeSum,
  XProgram,
  XImport,
  ImpPath,
) where

import Nuko.Tree.Expr     (Expr, XName, XTy)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe         (Maybe)

data LetDecl x = LetDecl
  { declName :: XName x
  , declArgs :: [(XName x, XTy x)]
  , declBody :: Expr x
  , declRet  :: XTy x
  , declExt  :: !(XLetDecl x)
  }

data TypeDeclArg x
  = TypeSym (XTy x)
  | TypeProd [(XName x, XTy x)]
  | TypeSum (NonEmpty (XName x, [XTy x]))

data TypeDecl x = TypeDecl
  { tyName :: XName x
  , tyArgs :: [XName x]
  , tyDecl :: !(TypeDeclArg x)
  }

type ImpPath x = NonEmpty (XName x)

data ImportDepsKind x
  = ImpDepLower (XName x)
  | ImpDepUpper (XName x)

data ImportDeps x = ImportDeps
  { name :: ImportDepsKind x
  , as   :: Maybe (XName x)
  }

data ImportModifier x
  = ImpAs (XName x)
  | ImpList (NonEmpty (ImportDeps x))
  | ImpStar

data Import x  = Import
  { path     :: ImpPath x
  , modifier :: Maybe (ImportModifier x)
  , impExt   :: !(XImport x)
  }

data Program x = Program
  { typeDecls  :: [TypeDecl x]
  , letDecls   :: [LetDecl x]
  , impDecls   :: [Import x]
  , programExt :: !(XProgram x)
  }

type family XLetDecl x
type family XProgram x
type family XTypeDecl x
type family XImport x

type family XTypeSym x
type family XTypeProd x
type family XTypeSum x
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
  XModName,
) where

import Nuko.Tree.Expr     (Expr, XName, XTy, XIdent, XModName)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe         (Maybe)
import Nuko.Names (ConsName, ValName, TyName)

data LetDecl x = LetDecl
  { declName :: XName x ValName
  , declArgs :: [(XName x ValName, XTy x)]
  , declBody :: Expr x
  , declRet  :: XTy x
  , declExt  :: !(XLetDecl x)
  }

data TypeDeclArg x
  = TypeSym (XTy x)
  | TypeProd [(XName x ValName, XTy x)]
  | TypeSum (NonEmpty (XName x ConsName, [XTy x]))

data TypeDecl x = TypeDecl
  { tyName :: XName x TyName
  , tyArgs :: [XName x TyName]
  , tyDecl :: !(TypeDeclArg x)
  }

data ImportDepsKind x
  = ImpDepLower (XIdent x)
  | ImpDepUpper (XIdent x)

data ImportDeps x = ImportDeps
  { name :: ImportDepsKind x
  , as   :: Maybe (XIdent x)
  }

data ImportModifier x
  = ImpAs (XIdent x)
  | ImpList (NonEmpty (ImportDeps x))
  | ImpStar

data Import x  = Import
  { path     :: XModName x
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
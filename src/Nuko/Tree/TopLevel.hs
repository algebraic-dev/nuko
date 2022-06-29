module Nuko.Tree.TopLevel (
  LetDecl(..),
  TypeDecl(..),
  Program(..),
  TypeDeclArg(..),
  Import(..),
  ImportTree(..),
  ImportDeps(..),
  ImportDepsKind(..),
  XLetDecl,
  XTypeDecl,
  XTypeProd,
  XTypeSym,
  XTypeSum,
  XProgram,
  XImport,
) where

import Nuko.Tree.Expr     (Expr, Ty, XName)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe         (Maybe)

data LetDecl x = LetDecl
  { declName :: XName x
  , declArgs :: [(XName x, Ty x)]
  , declBody :: Expr x
  , declRet  :: Maybe (Ty x)
  , declExt  :: !(XLetDecl x)
  }

data TypeDeclArg x
  = TypeSym (Ty x)
  | TypeProd [(XName x, Ty x)]
  | TypeSum (NonEmpty (XName x, [Ty x]))

data TypeDecl x = TypeDecl
  { tyName :: XName x
  , tyArgs :: [XName x]
  , tyDecl :: !(TypeDeclArg x)
  }

type ImpPath x = NonEmpty (XName x)

data ImportDepsKind x
  = ImpDepLower (XName x)
  | ImpDepUpper (XName x)

data ImportDeps x
  = ImpDepAs (ImportDepsKind x) (XName x)
  | ImpDep   (ImportDepsKind x)

data ImportTree x
  = ImpAs (ImpPath x) (XName x)
  | ImpList (ImpPath x) (NonEmpty (ImportDeps x))
  | Imp (ImpPath x)

data Import x  = Import
  { modName :: ImportTree x
  , impExt  :: !(XImport x)
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

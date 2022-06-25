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

import Nuko.Tree.Expr     (Expr, Name, Ty)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe         (Maybe)

data LetDecl x = LetDecl
  { declName :: Name x
  , declArgs :: [(Name x, Ty x)]
  , declBody :: Expr x
  , declRet  :: Maybe (Ty x)
  , declExt  :: !(XLetDecl x)
  }

data TypeDeclArg x
  = TypeSym (Ty x)
  | TypeProd [(Name x, Ty x)]
  | TypeSum (NonEmpty (Name x, [Ty x]))

data TypeDecl x = TypeDecl
  { tyName :: Name x
  , tyArgs :: [Name x]
  , tyDecl :: !(TypeDeclArg x)
  }

type ImpPath x = NonEmpty (Name x)

data ImportDepsKind x
  = ImpDepLower (Name x)
  | ImpDepUpper (Name x)

data ImportDeps x
  = ImpDepAs (ImportDepsKind x) (Name x)
  | ImpDep   (ImportDepsKind x)

data ImportTree x
  = ImpAs (ImpPath x) (Name x)
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
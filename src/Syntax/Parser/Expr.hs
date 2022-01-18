{-# LANGUAGE DeriveAnyClass #-}

module Syntax.Parser.Expr where 

import Syntax.Bounds
import Data.Text (Text)
import Data.List.NonEmpty

type Id = (WithBounds Text)
data Pat 
    = PId Id 
    | PLit Lit
    | PCons Id [Pat]
    deriving Show

data Type 
    = TArrow Bounds Type Type
    | TGen Id 
    | TSimple Id
    | TCons Bounds Id (NonEmpty Type)
    deriving Show


data Ann a = Ann Bounds a Type | Raw a 
    deriving Show
    
data Expr 
    = ECall Bounds Expr Expr
    | EBinary Bounds Id Expr Expr
    | ELambda Bounds (Ann Pat) Expr
    | EAssign Bounds (Ann Pat) Expr
    | ECase Bounds Expr [(Pat, Expr)]
    | ELit Lit
    | EId Id 
    | EBlock Bounds [Expr]
    deriving Show

data Lit 
    = LNumber Bounds Int
    | LString Bounds Text 
    deriving Show

data TypeDecl 
    = TDSumType (NonEmpty (Id, [Type]))
    | TDProductType [(Id, Type)]
    | TDSynonym Type
    deriving Show

data Decl 
    = DLet { letName :: Id, letArgs :: [Ann Pat], letBody :: Expr, letReturn :: Maybe Type }
    | DType { typeName :: Id, typeVars :: [Id], typeDecl :: TypeDecl }
    deriving Show 

data Program = Program [Int]

class HasPosition a where 
    getPos :: a -> Bounds

instance HasPosition Lit where 
    getPos (LNumber b _) = b
    getPos (LString b _) = b

instance HasPosition Pat where 
    getPos (PId id)  = getPos id 
    getPos (PLit lit) = getPos lit

instance HasPosition Expr where 
    getPos (ECall b _ _) = b
    getPos (EBinary b _ _ _) = b
    getPos (ELambda b _ _) = b 
    getPos (EAssign b _ _) = b 
    getPos (EId id) = bounds id 
    getPos (ELit lit) = getPos lit 
    getPos (EBlock b _) = b

instance HasPosition Type where 
    getPos (TArrow b _ _) = b 
    getPos (TGen id) = getPos id 
    getPos (TSimple id) = getPos id 

instance HasPosition (WithBounds a) where 
    getPos = bounds

instance HasPosition a => HasPosition (Ann a) where 
    getPos (Raw a) = getPos a
    getPos (Ann b _ _) = b
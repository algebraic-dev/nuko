module Type.Context where 
  

import Type.Types (RType(..), TypeKind(..), TypeId)

import qualified Data.List as List
import qualified Type.Types as Ty
import qualified Control.Arrow as Bi

data CtxElem 
  = CtxAlpha TypeId 
  | CtxVar TypeId (RType 'Poly)
  | CtxExists TypeId 
  | CtxSolved TypeId (RType 'Mono)
  | CtxMarker TypeId 
  deriving Eq 

instance Show CtxElem where 
  show = \case
    CtxAlpha s -> s
    CtxVar s ty -> s ++ " : " ++ show ty
    CtxExists s -> '^' : s
    CtxSolved s ty -> '^' : s ++ " = " ++ show ty 
    CtxMarker s -> "▶" ++ s

type Context = [CtxElem]

alphas :: Context -> [TypeId] 
alphas ctx = [x | CtxAlpha x <- ctx]

vars :: Context -> [TypeId] 
vars ctx = [x | CtxVar x _ <- ctx]

unsolved :: Context -> [TypeId] 
unsolved ctx = [x | CtxExists x <- ctx]

existentials :: Context -> [TypeId]
existentials ctx = ctx >>= go 
  where go (CtxExists x)   = [x]
        go (CtxSolved x _) = [x]
        go _ = []

markers :: Context -> [TypeId]
markers ctx = [x | CtxMarker x <- ctx]

findVar :: TypeId -> Context -> Maybe (RType 'Poly)
findVar name = \case 
  (CtxVar varName ty : xs) 
    | varName == name -> Just ty
    | otherwise -> findVar name xs   
  (_ : xs)            -> findVar name xs 
  []                  -> Nothing 

getSolved :: TypeId -> Context -> Maybe (RType 'Mono)
getSolved name (CtxSolved x ty : xs) 
  | x == name = Just ty  
  | otherwise = getSolved name xs
getSolved name (_ : xs) = getSolved name xs 
getSolved _    [] = Nothing 

breakMarker :: Context -> CtxElem -> (Context, Context)
breakMarker ctx tag = Bi.second tail $ List.span (/= tag) ctx

dropMarker :: Context -> CtxElem -> Context 
dropMarker ctx = snd . breakMarker ctx

applyContext :: Context -> RType 'Poly -> RType 'Poly 
applyContext ctx = \case 
  TyExists s  -> case getSolved s ctx of
                    Nothing -> TyExists s 
                    Just ty -> applyContext ctx (Ty.toPoly ty) 
  TyUnit        -> TyUnit 
  TyAlpha s     -> TyAlpha s 
  TyFun ty ty'  -> TyFun (applyContext ctx ty) (applyContext ctx ty')
  TyForall s ty -> TyForall s (applyContext ctx ty)

typeWellFormed :: Context -> RType 'Poly -> Bool 
typeWellFormed ctx = \case 
  TyUnit -> True
  TyAlpha s  | s `elem` alphas ctx -> True  
             | otherwise -> False 
  TyExists s | s `elem` existentials ctx -> True 
             | otherwise -> False
  TyFun ty ty' -> typeWellFormed ctx ty && typeWellFormed ctx ty'
  TyForall s ty -> typeWellFormed (CtxAlpha s : ctx) ty

solve :: Context -> TypeId -> RType 'Mono -> Context 
solve ctx ex ty = let (l, r) = breakMarker ctx (CtxExists ex) in l <> (CtxSolved ex ty : r)

insertAt :: Context -> CtxElem -> [CtxElem] -> Context 
insertAt ctx ex ty = let (l, r) = breakMarker ctx ex in l <> ty <> r


isOrdered :: Context -> TypeId -> TypeId -> Bool 
isOrdered ctx a b = a `elem` existentials (dropMarker ctx (CtxExists b))
module Nuko.Typer.Infer.Common (
    seqPattern,
    inferPattern,
    checkPattern
) where

import Nuko.Typer.Env
import Nuko.Tree.Expr         hiding (Name)
import Nuko.Typer.Infer.Pat   (inferLit)
import Nuko.Typer.Types       (Ty(TyHole, TyFun))
import Nuko.Syntax.Ast        (Normal)
import Nuko.Typer.Unify       (unify)
import Nuko.Typer.Infer.Type  (inferTy)

import qualified Data.Map     as Map

seqPattern :: TyperMonad m => [Pat Normal] -> (Map.Map Name Ty) -> m ([Ty], Map.Map Name Ty)
seqPattern [] ids = pure ([], ids)
seqPattern (x : xs) ids = do
  (resTy, ids') <- inferPattern x ids
  (resTys, ids'') <- seqPattern xs ids'
  pure (resTy : resTys, ids'')

inferPattern :: TyperMonad m => Pat Normal -> (Map.Map Name Ty) -> m (Ty, Map.Map Name Ty)
inferPattern pat ids = track (InInferPat pat) $ case pat of
  PWild range -> do
    hole <- TyHole range <$> newHole
    pure (hole, ids)
  PCons name patterns range -> do
    consTy <- undefined
    retTy <- TyHole range <$> newHole
    (patTys, ids') <- seqPattern patterns ids
    unify (foldr (TyFun range) retTy patTys) consTy
    pure (retTy, ids')
  PId name _ -> do
    idTy <- TyHole name.loc <$> newHole
    pure (idTy, Map.insert name.ident idTy ids)
  PLit lit _ -> do
    resTy <- inferLit lit
    pure (resTy, ids)
  PAnn pat' ty _ -> do
    inferredTy <- inferTy ty
    ids' <- checkPattern pat' inferredTy ids
    pure (inferredTy, ids')
  PExt _ -> undefined

checkPattern :: TyperMonad m => Pat Normal -> Ty -> (Map.Map Name Ty) -> m (Map.Map Name Ty)
checkPattern pat ty ids = track (InCheckPat pat) $ do
  (inferredPat, ids') <- inferPattern pat ids
  unify inferredPat ty
  pure ids'

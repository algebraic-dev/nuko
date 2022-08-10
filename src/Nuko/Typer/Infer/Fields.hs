module Nuko.Typer.Infer.Fields (
  assertFields
) where

import Relude

import Nuko.Names          (Name, Qualified, TyName, ValName)
import Nuko.Report.Range   (HasPosition (getPos), Range (..))
import Nuko.Tree           (Re)
import Nuko.Tree.Expr      (RecordBinder, rbName)
import Nuko.Typer.Env      (FieldInfo (_fiResultType), MonadTyper,
                            TyInfo (_resultantType, _tyNames), endDiagnostic,
                            getTy, newTyHole, tsTypes)
import Nuko.Typer.Error    (TypeError (..))
import Nuko.Typer.Types    (Relation (Virtual), TTy, evaluate)
import Nuko.Typer.Unify    (destructFun, unify)

import Data.Traversable    (for)

import Data.HashMap.Strict qualified as HashMap
import Data.List           qualified as List

-- Removes duplicated fields and things that does not belongs to the type
assertFields :: (MonadTyper m, HasPosition (RecordBinder val Re)) => Qualified (Name TyName) -> Range -> HashMap (Name ValName) FieldInfo -> [RecordBinder val Re] -> m ([(RecordBinder val Re, TTy 'Virtual)], TTy 'Virtual)
assertFields qualified range resInfo binders = do
  tyInfo    <- snd <$> getTy tsTypes qualified
  holes    <- traverse newTyHole (fst <$> tyInfo._tyNames)
  let resTy = evaluate holes tyInfo._resultantType
  let parts = List.groupBy ((==) `on` rbName) $ List.sortBy (compare `on` rbName) binders
  newBinders' <-
    for parts $ \group' ->
      case group' of
        []    -> error "Impossible case!"
        [binder] -> do
          let fieldTy = (evaluate [] . _fiResultType) <$> HashMap.lookup binder.rbName resInfo
          case fieldTy of
            Nothing -> endDiagnostic (FieldDoesNotBelongsToTheType binder.rbName) range
            Just ty -> do
              (argTy, retTy) <- destructFun (getPos binder) ty
              _ <- unify (getPos binder) resTy retTy
              pure (binder, argTy)
        (other:_) -> endDiagnostic (DuplicatedFieldName other.rbName) range
  pure (newBinders', resTy)

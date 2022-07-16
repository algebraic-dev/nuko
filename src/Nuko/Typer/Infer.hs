module Nuko.Typer.Infer (
  inferProgram
) where

import Relude.Foldable           (traverse)
import Relude.Container          (uncurry)
import Relude.List               (zip)
import Relude.Applicative        (pure)

import Nuko.Tree                 (TypeDecl, Tc, Program (..), Re)
import Nuko.Typer.Env            (MonadTyper)
import Nuko.Typer.Infer.TypeDecl (initTypeDecl, inferTypeDecl)

inferProgram :: MonadTyper m => Program Re -> m [TypeDecl Tc]
inferProgram program = do
  initDatas <- traverse initTypeDecl program.typeDecls
  letDecls  <- traverse (uncurry inferTypeDecl) (zip program.typeDecls initDatas)
  pure letDecls
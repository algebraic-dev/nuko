module Nuko.Typer.Infer (
  inferProgram
) where

import Relude

import Nuko.Tree                 (NoExt (NoExt), Program (..), Re, Tc)
import Nuko.Typer.Env            (MonadTyper)
import Nuko.Typer.Infer.LetDecl  (inferLetDecl, initLetDecl)
import Nuko.Typer.Infer.TypeDecl (inferTypeDecl, initTypeDecl)

inferProgram :: MonadTyper m => Program Re -> m (Program Tc)
inferProgram program = do
  initDatas <- traverse initTypeDecl program.typeDecls
  typDecls  <- traverse (uncurry inferTypeDecl) (zip program.typeDecls initDatas)
  letDatas  <- traverse initLetDecl program.letDecls
  letDecls  <- traverse (uncurry inferLetDecl) (zip program.letDecls letDatas)
  pure (Program typDecls letDecls [] NoExt)

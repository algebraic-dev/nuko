module Nuko.Typer.Infer (
  inferProgram
) where

import Relude.Foldable           (traverse)
import Relude.Container          (uncurry)
import Relude.List               (zip)
import Relude.Applicative        (pure)

import Nuko.Tree                 (Tc, Program (..), Re, NoExt (NoExt))
import Nuko.Typer.Env            (MonadTyper)
import Nuko.Typer.Infer.TypeDecl (initTypeDecl, inferTypeDecl)
import Nuko.Typer.Infer.LetDecl (initLetDecl, inferLetDecl)

inferProgram :: MonadTyper m => Program Re -> m (Program Tc)
inferProgram program = do
  initDatas <- traverse initTypeDecl program.typeDecls
  typDecls  <- traverse (uncurry inferTypeDecl) (zip program.typeDecls initDatas)
  letDatas  <- traverse initLetDecl program.letDecls
  letDecls  <- traverse (uncurry inferLetDecl) (zip program.letDecls letDatas)
  pure (Program typDecls letDecls [] NoExt)
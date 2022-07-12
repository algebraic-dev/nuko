module Nuko.Typer.Infer (
  initTypeDecl,
  inferTypeDecl,
  checkTypeSymLoop,
  inferProgram
) where

import Nuko.Typer.Infer.TypeDecl (InitTypeData(itCanonName, itResKind), checkTypeSymLoop, initTypeDecl, inferTypeDecl )
import Nuko.Typer.Env            (MonadTyper, updateTyKind)
import Nuko.Tree                 (Program(..), Re, Tc, NoExt (NoExt))

import Relude.Foldable    (Traversable(traverse), for_)
import Relude.Container   (uncurry)
import Relude.List        (zip)
import Relude.Monad       ((>>=), Maybe (..))
import Relude.Function    (($))
import Relude.Applicative (Applicative(pure))

import Nuko.Typer.Types (removeKindHoles, dereferenceKind)
import Relude.Debug (undefined)

inferProgram :: MonadTyper m => Program Re -> m (Program Tc)
inferProgram program = do

  -- Inference of all the type declarations.
  initDatas <- traverse initTypeDecl program.typeDecls
  checkTypeSymLoop program.typeDecls
  checkedTypeDecls <- traverse (uncurry inferTypeDecl) (zip program.typeDecls initDatas)
  for_ initDatas $ \initData -> do
    res <- dereferenceKind initData.itResKind >>= removeKindHoles
    updateTyKind initData.itCanonName (\(_, i) -> Just (res, i))

  checkedLetDecls <- undefined

  pure (Program checkedTypeDecls checkedLetDecls [] NoExt)
module Nuko.Resolver where

import Relude                (undefined, NonEmpty, Semigroup ((<>)), Monad ((>>=)), for_)
import Relude.String         (Text)
import Relude.List           (NonEmpty((:|)))
import Nuko.Resolver.Error   ()
import Nuko.Resolver.Support (MonadResolver, importModule, addImport, ModuleSig (_moduleName, ModuleSig))
import Nuko.Tree             (Nuko, Phase(Normal, Resolved))
import Nuko.Syntax.Tree      (Name(..))
import Nuko.Syntax.Range     (Range)
import Lens.Micro.Platform   (Lens')
import Nuko.Tree.TopLevel

type Nm = Nuko 'Normal
type Re = Nuko 'Resolved

joinPath :: NonEmpty Name -> Name
joinPath (y :| ys) =
    join' ys y.text y.range
  where
    join' :: [Name] -> Text -> Range -> Name
    join' []  ls s      = Name ls s
    join' [x] ls s      = Name (ls <> "." <> x.text) (s <> x.range)
    join' (x : xs) ls s = join' xs (ls <> "." <> x.text) s

resolveImportTree :: MonadResolver m => ImportTree Nm -> m ()
resolveImportTree = \case
    ImpAs path as      -> do
      importPath path >>= addImport as.text
    Imp path           -> do
      mod' <- importPath path
      addImport mod'._moduleName mod'
    ImpList path items -> do
      mod' <- importPath path
      for_ items (importItem mod')
  where
    importPath :: MonadResolver m => NonEmpty Name -> m ModuleSig
    importPath path = let name = joinPath path in importModule name.range name.text

    importItem :: MonadResolver m => ModuleSig -> ImportDeps Nm -> m ()
    importItem mod' = \case
      ImpDep kind      -> undefined
      ImpDepAs kind as -> undefined

resolveImport :: MonadResolver m => Import Nm -> m ()
resolveImport (Import tree _) = resolveImportTree tree
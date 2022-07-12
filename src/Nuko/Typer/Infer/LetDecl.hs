module Nuko.Typer.Infer.LetDecl (
  initLetDecl,
  inferLetDecl
) where

import Relude                (newIORef, snd, fst, Foldable (foldl', length), writeIORef, Applicative ((*>)), ($), HashMap, gets, Semigroup ((<>)), zip, undefined, Num ((+)), print, traceShowId)
import Relude.String         (Text)
import Relude.Functor        (Functor(fmap), (<$>))
import Relude.Foldable       (Foldable(foldr), Traversable(traverse), traverse_, for_)
import Relude.Applicative    (Applicative(pure))

import Nuko.Typer.Tree       () -- Just to make the equality between XName Re and XName Tc works
import Nuko.Tree             (LetDecl(..), Re, Tc, Ty, XName, XTy)
import Nuko.Typer.Env        (MonadTyper, addTyKind, addTy, tsConstructors, TyInfo (IsTySyn, IsTyDef), addFieldToEnv, FieldInfo (FieldInfo), TypingEnv (_teCurModule), newTyHole, newKindHole, tsVars)
import Nuko.Typer.Types      (Hole (..), TKind (..), TTy (..), Virtual, KiHole, implTy, generalizeOver)
import Nuko.Resolver.Tree    (ReId(text, ReId), Path (Local))
import Nuko.Tree.TopLevel
import Nuko.Typer.Infer.Type (inferTy, findCycle, freeVars)
import Nuko.Typer.Unify      (unifyKind)

import qualified Data.HashMap.Strict as HashMap
import Nuko.Report.Range (emptyRange)
import qualified Data.HashSet as HashSet

initLetDecl :: MonadTyper m => LetDecl Re -> m ()
initLetDecl decl = do

  let argRawTypes = fmap snd decl.declArgs
  freeVarsSet    <- traceShowId <$> (HashSet.toList <$> foldr (<>) HashSet.empty <$> traverse freeVars argRawTypes)
  newVars        <- traverse newKindHole freeVarsSet
  let bindings    = zip freeVarsSet newVars
  argsBindings   <- traverse (inferTy bindings) argRawTypes
  traverse_ (`unifyKind` KiStar) (snd <$> argsBindings)

  (retType, retKind) <- inferTy bindings decl.declRet
  unifyKind retKind KiStar

  let fnType = foldr TyFun retType (fst <$> argsBindings)

  curMod <- gets _teCurModule
  let canonName = curMod <> "." <> decl.declName.text

  let generalizedTy = generalizeOver freeVarsSet fnType

  addTy tsVars canonName generalizedTy

  pure ()

inferLetDecl :: MonadTyper m => LetDecl Re -> m (LetDecl Tc)
inferLetDecl (LetDecl name args ret body e) = undefined
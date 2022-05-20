module Nuko.Typer.Infer.Pat (
    inferLit
) where

import Nuko.Typer.Env   ( named, track, Tracker(InInferLit), TyperMonad )
import Nuko.Tree.Expr   ( Literal(..) )
import Nuko.Typer.Types ( Ty )
import Nuko.Syntax.Ast  ( Normal )

inferLit :: TyperMonad m => Literal Normal -> m Ty
inferLit lit = track (InInferLit lit) $ case lit of
  LStr _ loc' -> named loc' "String"
  LInt _ loc' -> named loc' "Int"

module Nuko.Resolver.Path (
  resolveName,
  resolvePath,
  resolveQualified,
  resolveConsOrTy,
  resolveInNameSpace,
  useLocalPath,
  getPublicLabels,
  qualifyPath,
  getPathData,
) where

import Relude

import Nuko.Names
import Nuko.Report.Range        (SetPosition (setPos), copyPos, getPos)
import Nuko.Report.Text         (Severity (Error))
import Nuko.Resolver.Env        (MonadResolver, useModule)
import Nuko.Resolver.Env        qualified as Env
import Nuko.Resolver.Error      (ResolveErrorReason (..))
import Nuko.Resolver.Occourence (OccEnv (..), getMap)
import Nuko.Utils               (terminate)

import Data.HashMap.Strict      qualified as HashMap
import Lens.Micro.Platform      (at, use, view)

occAt :: Functor f => Label -> (Maybe a2 -> f (Maybe a2)) -> OccEnv a2 -> f (OccEnv a2)
occAt l = getMap . at l

useLocalPath :: MonadResolver m => Name k -> m (Maybe (Name k))
useLocalPath name' = do
  res <- use (Env.localNames . occAt (Label name'))
  pure (coerceLabel name'.nKind <$> res)

useGlobal :: MonadResolver m => Env.NameSpace -> Name k -> m (Maybe (Qualified (Name k), Env.Visibility))
useGlobal ns name' = do
  let visibilityRes = view (Env.names . occAt (Label name')) ns
  pure ((attachModName ns._modName name', ) . fst <$> visibilityRes)

getPathData :: MonadResolver m => Path (Name k) -> m (Maybe (Env.Visibility, Env.DefType))
getPathData path = do
  ns <- case path of
        Full _ q  -> useModule q.qModule
        Local _ _ -> use Env.currentNamespace
  let info = getPathInfo path
  pure $ view (Env.names . occAt (Label info)) ns

-- | TODO: Add each name in the openedNames so it will make ambiguity checking easier.
useGlobalPath :: MonadResolver m => Env.NameSpace -> Name k -> m (Maybe (Path (Name k)))
useGlobalPath ns name' = do
  result <- useGlobal ns name'
  pure (mkQualifiedPath . fst <$> result)

useOpenedPath :: MonadResolver m => Name k -> m (Maybe (Path (Name k)))
useOpenedPath name' = do
    res <- use (Env.openedNames . occAt (Label name'))
    mapM (resolveAmbiguity name') res
  where
    resolveAmbiguity :: MonadResolver m => Name k -> Env.Use -> m (Path (Name k))
    resolveAmbiguity name'' = \case
      (Env.Ambiguous refs) -> terminate =<< Env.mkDiagnostic Error (getPos name'') (AmbiguousNames (getPos name'') refs)
      (Env.Single resPath) ->
        let fixedPos = copyPos name'' (copyPos name'' <$> resPath) in
        pure (mkQualifiedPath (coerceLabel name''.nKind <$> fixedPos)) -- I wish I had dependent hashmaps :P

resolveName :: MonadResolver m => Name k -> m (Path (Name k))
resolveName name' = do
  module' <- use Env.currentNamespace
  res  <- sequence [(mkLocalPath <$>) <$> useLocalPath name', useGlobalPath module' name', useOpenedPath name']
  path <- assertLookup (one (NameSort name'.nKind)) name'.nIdent Nothing (asum res)
  pure $ setPos (getPos name') path

resolveInNameSpace :: MonadResolver m => Env.NameSpace -> Name k -> m (Qualified (Name k))
resolveInNameSpace module' name' = do
  res <- useGlobal module' name'
  (path, vis) <- assertLookup (one (NameSort name'.nKind)) name'.nIdent Nothing res
  assertVisibility (Label <$> mkQualifiedPath path) vis
  pure path

assertLookup :: MonadResolver m => NonEmpty NameSort -> Ident -> Maybe ModName -> Maybe a -> m a
assertLookup sorts ident onModule res = do
  case res of
    Just res' -> pure res'
    Nothing   -> terminate =<< Env.mkDiagnostic Error (getPos ident) (CannotFindInModule sorts (mkPath onModule ident))

resolveQualified :: MonadResolver m => Qualified (Name k) -> m (Qualified (Name k))
resolveQualified qualified = do
  module'     <- Env.useModule qualified.qModule
  result      <- useGlobal module' qualified.qInfo
  (path, vis) <- assertLookup (one (NameSort qualified.qInfo.nKind)) (qualified.qInfo.nIdent) (Just qualified.qModule) result
  assertVisibility (Label <$> mkQualifiedPath path) vis
  pure (setPos (getPos qualified) path)

resolvePath :: MonadResolver m => Path (Name k) -> m (Path (Name k))
resolvePath (Local _ path)     = resolveName path
resolvePath (Full _ qualified) = mkQualifiedPath <$> resolveQualified qualified

assertVisibility :: MonadResolver m => Path Label -> Env.Visibility -> m ()
assertVisibility name = \case
  Env.Public  -> pure ()
  Env.Private -> terminate =<< Env.mkDiagnostic Error (getPos name) (IsPrivate name)

resolveConsOrTy :: MonadResolver m => Env.NameSpace -> Ident -> m (Qualified Label)
resolveConsOrTy ns ident = do
    resolveKind TyName $
      resolveKind ConsName $
        terminate =<< Env.mkDiagnostic Error (getPos ident) (CannotFindInModule (fromList [NameSort TyName, NameSort ConsName]) (mkPath (Just ns._modName) ident))
  where
    resolveKind :: MonadResolver m => NameKind k -> m (Qualified Label) -> m (Qualified Label)
    resolveKind kind action = do
      result <- useGlobal ns (mkName kind ident Untouched)
      case result of
        Just (qual, vis) -> do
          assertVisibility (Label <$> mkQualifiedPath qual) vis
          pure (Label <$> qual)
        Nothing  -> action

getPublicLabels :: Env.NameSpace -> [Label]
getPublicLabels ns =
    fmap fst
  $ filter (\(_, v) -> fst v == Env.Public)
  $ HashMap.toList (ns._names._getMap)

qualifyPath :: MonadResolver m => Path (Name k) -> m (Qualified (Name k))
qualifyPath = \case
  Full _ q -> pure q
  Local _ n -> do
    module' <- use Env.currentNamespace
    pure (mkQualified module'._modName n (getPos n))

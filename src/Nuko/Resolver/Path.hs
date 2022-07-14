module Nuko.Resolver.Path (
  resolveName,
  resolvePath,
  resolveQualified,
  resolveConsOrTy,
  resolveInNameSpace,
  useLocalPath,
  getPublicLabels,
) where

import Nuko.Names
import Nuko.Resolver.Env
import Nuko.Resolver.Occourence (getMap, OccEnv(..))
import Nuko.Resolver.Error      (ResolveErrorReason (..), mkErr)
import Nuko.Report.Range        (copyPos, getPos)
import Nuko.Utils               (terminate)

import Relude                   ((.), asum, NonEmpty, fst, ($), filter, Eq ((==)), Functor (fmap))
import Relude.Monad             (Maybe(..))
import Relude.Functor           ((<$>), Functor ((<$)))
import Relude.Applicative       (Applicative(pure))
import Relude.Container         (one, fromList)
import Relude.Foldable          (Traversable(..))

import Lens.Micro.Platform      (use, at, view)

import qualified Data.HashMap.Strict as HashMap

occAt :: Functor f => Label -> (Maybe a2 -> f (Maybe a2)) -> OccEnv a2 -> f (OccEnv a2)
occAt l = getMap . at l

useLocalPath :: MonadResolver m => Name k -> m (Maybe (Path (Name k)))
useLocalPath name' = do
  res <- use (localNames . occAt (Label name'))
  pure (mkLocalPath name' <$ res)

useGlobal :: MonadResolver m => NameSpace -> Name k -> m (Maybe (Qualified (Name k), Visibility))
useGlobal ns name' = do
  let visibilityRes = view (names . occAt (Label name')) ns
  pure ((attachModName ns._modName name', ) <$> visibilityRes)

-- | basically DEPRECATED, i'll add each name in the openedNames twice so it would make ambiguity checking
-- easier.
useGlobalPath :: MonadResolver m => NameSpace -> Name k -> m (Maybe (Path (Name k)))
useGlobalPath ns name' = do
  result <- useGlobal ns name'
  pure (mkQualifiedPath . fst <$> result)

useOpenedPath :: MonadResolver m => Name k -> m (Maybe (Path (Name k)))
useOpenedPath name' = do
    res <- use (openedNames . occAt (Label name'))
    mapM (resolveAmbiguity name') res
  where
    resolveAmbiguity :: MonadResolver m => Name k -> Use -> m (Path (Name k))
    resolveAmbiguity name'' = \case
      (Ambiguous refs) -> terminate (mkErr $ AmbiguousNames (getPos name'') refs)
      (Single resPath) ->
        let fixedPos = copyPos name'' (copyPos name'' <$> resPath) in
        pure (mkQualifiedPath (coerceLabel name''.nKind <$> fixedPos)) -- I wish I had dependent hashmaps :P

resolveName :: MonadResolver m => Name k -> m (Path (Name k))
resolveName name' = do
  module' <- use currentNamespace
  res  <- sequence [useLocalPath name', useGlobalPath module' name', useOpenedPath name']
  assertLookup (one (NameSort name'.nKind)) name'.nIdent Nothing (asum res)

resolveInNameSpace :: MonadResolver m => NameSpace -> Name k -> m (Qualified (Name k))
resolveInNameSpace module' name' = do
  res <- useGlobal module' name'
  (path, vis) <- assertLookup (one (NameSort name'.nKind)) name'.nIdent Nothing res
  assertVisibility (Label <$> mkQualifiedPath path) vis
  pure path

assertLookup :: MonadResolver m => NonEmpty NameSort -> Ident -> Maybe ModName -> Maybe a -> m a
assertLookup sorts ident on res = do
  case res of
    Just res' -> pure res'
    Nothing   -> terminate (mkErr $ CannotFindInModule sorts (mkPath on ident))

resolveQualified :: MonadResolver m => Qualified (Name k) -> m (Qualified (Name k))
resolveQualified qualified = do
  module'     <- useModule qualified.qModule
  result      <- useGlobal module' qualified.qInfo
  (path, vis) <- assertLookup (one (NameSort qualified.qInfo.nKind)) (qualified.qInfo.nIdent) (Just qualified.qModule) result
  assertVisibility (Label <$> mkQualifiedPath path) vis
  pure path

resolvePath :: MonadResolver m => Path (Name k) -> m (Path (Name k))
resolvePath (Local _ path)     = resolveName path
resolvePath (Full _ qualified) = mkQualifiedPath <$> resolveQualified qualified

assertVisibility :: MonadResolver m => Path Label -> Visibility -> m ()
assertVisibility name = \case
  Public  -> pure ()
  Private -> terminate (mkErr $ IsPrivate name)

resolveConsOrTy :: MonadResolver m => NameSpace -> Ident -> m (Qualified Label)
resolveConsOrTy ns ident = do
    resolveKind TyName $
      resolveKind ConsName $
        terminate (mkErr $ CannotFindInModule (fromList [NameSort TyName, NameSort ConsName]) (mkPath (Just ns._modName) ident))
  where
    resolveKind :: MonadResolver m => NameKind k -> m (Qualified Label) -> m (Qualified Label)
    resolveKind kind action = do
      result <- useGlobal ns (mkName kind ident Untouched)
      case result of
        Just (qual, vis) -> do
          assertVisibility (Label <$> mkQualifiedPath qual) vis
          pure (Label <$> qual)
        Nothing  -> action

getPublicLabels :: NameSpace -> [Label]
getPublicLabels ns =
    fmap fst
  $ filter (\(_, v) -> v == Public)
  $ HashMap.toList (ns._names._getMap)
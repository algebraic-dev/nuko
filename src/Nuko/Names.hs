module Nuko.Names (
  Attribute(..),
  NameSort(..),
  Ident(..),
  Path(..),
  Name(..),
  ModName(..),
  Qualified(..),
  NameKind(..),
  Label(..),
  ValName,
  TyName,
  ConsName,
  MiscName,
  mkIdent,
  mkName,
  mkLabel,
  mkModName,
  mkQualified,
  mkQualifiedPath,
  attachModName,
  mkQualifiedWithPos,
  mkLocalPath,
  mkValName,
  mkTyName,
  mkConsName,
  getIdent,
  getNameSort,
  coerceTo,
  addSegments,
  changeName,
  changeLabel,
  genIdent,
  coerceLabel,
  getPathModule,
  getPathInfo,
  moduleLastIdent,
  getChildName,
  mkPath,
) where

import Relude            (Bool (..), Int, Semigroup(..), NonEmpty ((:|)), Maybe (..), last, show)
import Relude.Base       (Generic, Eq ((==)))
import Relude.String     (Text)
import Relude.Functor    (Functor(fmap))
import Relude.Foldable   (Foldable(toList))
import Relude.Function   (($))

import Data.Text         (intercalate)
import Pretty.Tree       (PrettyTree(..), Tree (..))
import Data.Hashable     (Hashable(hash, hashWithSalt))
import Nuko.Report.Range (Range, toLabel, HasPosition (getPos), emptyRange, SetPosition(..))
import Pretty.Format     (Format(..))

data ValName
data TyName
data ConsName
data MiscName

-- | This is simply some aditional information
data IdentAttr
  = Generated
  | FromSource

data Ident = Ident
  { iHash   :: {-# UNPACK #-}!Int
  , iText   :: Text
  , iRange  :: Range
  , iAttr   :: IdentAttr
  } deriving stock Generic

data NameKind k where
  ValName   :: NameKind ValName
  TyName    :: NameKind TyName
  ConsName  :: NameKind ConsName

data NameSort = forall k. NameSort (NameKind k)

-- | This is simply some aditional information
data Attribute
  = Untouched
  | Was Ident

data Name e = Name
  { nHash   :: {-# UNPACK #-}!Int
  , nKind   :: NameKind e
  , nAttr   :: Attribute
  , nIdent  :: Ident
  } deriving stock Generic

-- | A Label is just a type synonym for the
-- impredicative type of anonymous kind name
data Label = forall a. Label { lName :: Name a }

-- | The path of a module
data ModName = ModName
  { mHash'    :: {-# UNPACK #-}!Int
  , mSegments :: NonEmpty Ident
  , mRange    :: Range
  } deriving stock Generic

-- | A name that is qualified by a module name
data Qualified a = Qualified
  { qHash   :: {-# UNPACK #-}!Int
  , qModule :: ModName
  , qInfo   :: a
  , qRange  :: Range
  } deriving stock (Generic, Functor)

-- | Paths that can be either local (like let declarations) or
-- external.
data Path e
  = Full {-# UNPACK #-}!Int (Qualified e)
  | Local {-# UNPACK #-}!Int e
  deriving stock (Functor)

getNameSort :: Label -> NameSort
getNameSort (Label name) = NameSort name.nKind

getIdent :: Label -> Ident
getIdent (Label name) = name.nIdent

genIdent :: Text -> Ident
genIdent text = Ident (hash text) text emptyRange Generated

mkIdent :: Text -> Range -> Ident
mkIdent text r = Ident (hash text) text r FromSource

mkName :: NameKind e -> Ident -> Attribute -> Name e
mkName kind text attr = Name (hash text `hashWithSalt` hash kind) kind attr text

mkLabel :: NameKind e -> Ident -> Attribute -> Label
mkLabel kind text attr = Label (mkName kind text attr)

mkModName :: NonEmpty Ident -> ModName
mkModName path = ModName (hash path) path (getPos path)

mkQualified :: Hashable a => ModName -> a -> Range -> Qualified a
mkQualified mod text = Qualified (hash mod `hashWithSalt` text) mod text

mkQualifiedWithPos :: (Hashable a, HasPosition a) => ModName -> a -> Qualified a
mkQualifiedWithPos mod text = Qualified (hash mod `hashWithSalt` text) mod text (getPos mod <> getPos text)

attachModName :: Hashable a => HasPosition a => ModName -> a -> Qualified a
attachModName modName info = mkQualified modName info (getPos info)

mkQualifiedPath :: Qualified a -> Path a
mkQualifiedPath qual = Full (hash qual) qual

mkLocalPath :: Hashable a => a -> Path a
mkLocalPath name = Local (hash name) name

mkTyName :: Ident -> Name TyName
mkTyName ident = mkName TyName ident Untouched

mkValName :: Ident -> Name ValName
mkValName ident = mkName ValName ident Untouched

mkConsName :: Ident -> Name ConsName
mkConsName ident = mkName ConsName ident Untouched

changeName :: Ident -> Name x -> Name x
changeName to (Name _ kind Untouched ident) = mkName kind to (Was ident)
changeName to (Name _ kind other         _) = mkName kind to other

changeLabel :: Ident -> Label -> Label
changeLabel to (Label n) = Label (changeName to n)

coerceTo :: NameKind k' -> Name k -> Name k'
coerceTo newKind (Name _ _ attr ident) = mkName newKind ident attr

coerceLabel :: NameKind k' -> Label -> Name k'
coerceLabel newKind (Label (Name _ _ attr ident)) = mkName newKind ident attr

addSegments :: ModName -> [Ident] -> ModName
addSegments (ModName _ (x :| seg) r) toAppend =
  let segs = (x :| seg <> toAppend) in
  ModName (hash segs) segs r

getPathModule :: Path a -> Maybe ModName
getPathModule (Local _ _) = Nothing
getPathModule (Full _ qual) = Just qual.qModule

getPathInfo :: Path a -> a
getPathInfo (Local _ ident) = ident
getPathInfo (Full _ qual) = qual.qInfo

moduleLastIdent :: ModName -> Ident
moduleLastIdent (ModName _ segments _) = last segments

getChildName :: ModName -> ModName
getChildName (ModName _ segments _) = mkModName (last segments :| [])

mkPath :: (Hashable a, HasPosition a) => Maybe ModName -> a -> Path a
mkPath Nothing ident = mkLocalPath ident
mkPath (Just modName) ident = mkQualifiedPath (mkQualifiedWithPos modName ident)

-- Instances for comparisons and hashing

instance Hashable Label where
  hash (Label e) = hash e
  hashWithSalt salt (Label e) = hashWithSalt salt e

instance Hashable Ident where
  hash (Ident hash' _ _ _) = hash'
  hashWithSalt salt (Ident hash' _ _ _) = hashWithSalt salt hash'

instance Hashable (NameKind k) where
  hash = \case
    ValName   -> hash (1 :: Int)
    TyName    -> hash (2 :: Int)
    ConsName  -> hash (3 :: Int)
  hashWithSalt salt n = salt `hashWithSalt` hash n

instance Hashable (Name k) where
  hash (Name hash' _ _ _) = hash'
  hashWithSalt salt (Name hash' _ _ _) = hashWithSalt salt hash'

instance Hashable ModName where
  hash (ModName hash' _ _) = hash'
  hashWithSalt salt (ModName hash' _ _) = hashWithSalt salt hash'

instance Hashable (Qualified a) where
  hash (Qualified hash' _ _ _) = hash'
  hashWithSalt salt (Qualified hash' _ _ _) = hashWithSalt salt hash'

instance Hashable (Path a) where
  hash (Full hash' _) = hash'
  hash (Local hash' _) = hash'
  hashWithSalt salt (Full hash' _) = hashWithSalt salt hash'
  hashWithSalt salt (Local hash' _) = hashWithSalt salt hash'

instance Eq Label where
  (Label name) == (Label name') = name.nHash == name'.nHash

instance Eq (Name k) where
  (Name h _ _ _) == (Name h' _ _ _) = h == h'

instance Eq Ident where
  (Ident hash' _ _ _) == (Ident hash'' _ _ _) = hash' == hash''

instance Eq ModName where
  (ModName hash' _ _) == (ModName hash'' _ _) = hash' == hash''

instance Eq (Qualified a) where
  (Qualified hash' _ _ _) == (Qualified hash'' _ _ _) = hash' == hash''

instance Eq (Path a) where
  (Full hash' _) == (Full hash'' _) = hash' == hash''
  (Local hash' _) == (Local hash'' _) = hash' == hash''
  _ == _ = False

-- Instances for pretty printing

joinSegments :: NonEmpty Ident -> Text
joinSegments segments = intercalate "." (toList $ fmap iText segments)

showKind :: NameKind k -> Text
showKind = \case
  ValName   -> "ValName"
  TyName    -> "TyName"
  ConsName  -> "ConsName"

instance PrettyTree Label where
  prettyTree (Label n) = prettyTree n

instance PrettyTree Ident where
  prettyTree (Ident _ text range' _) =
    Node "Ident:" [text, toLabel range'] []

instance PrettyTree ModName where
  prettyTree (ModName _ segments _) =
    Node "ModName:" [joinSegments segments] []

instance PrettyTree (Name k) where
  prettyTree (Name _ kind _ (Ident _ text range' _)) =
      Node "Name:" [showKind kind, text, toLabel range'] []

instance PrettyTree a => PrettyTree (Qualified a) where
  prettyTree (Qualified _ (ModName _ segments _) name range') =
    Node "Qualified:" [joinSegments segments, toLabel range'] [prettyTree name]

instance PrettyTree a => PrettyTree (Path a) where
  prettyTree (Local _ name) = Node "Local:" [] [prettyTree name]
  prettyTree (Full _ (Qualified _ (ModName _ segments _) name range')) =
    Node "Full:" [show $ joinSegments segments, toLabel range'] [prettyTree name]

instance PrettyTree NameSort where
  prettyTree (NameSort a) = Node "NameSort" [showKind a] []

instance Format Ident where
  format ident = ident.iText

instance Format (Name k) where
  format (Name _ _ Untouched ident) = format ident
  format (Name _ _ (Was ident) _) = format ident

instance Format (NameKind k) where
  format = \case
    ValName   -> "value"
    TyName   -> "type"
    ConsName  -> "constructor"

instance Format NameSort where
  format (NameSort sort) = format sort

instance Format Label where
  format (Label name) = format name

instance Format ModName where
  format (ModName _ seg _) = joinSegments seg

instance Format k => Format (Qualified k) where
  format (Qualified _ modName n _) = format modName <> "." <> format n

instance Format k => Format (Path k) where
  format (Full _ qualified) = format qualified
  format (Local _ name) = format name

-- Instances for source code position

instance HasPosition Ident where
  getPos (Ident _ _ range' _) = range'

instance HasPosition (Name k) where
  getPos (Name _ _ _ range') = getPos range'

instance HasPosition Label where
  getPos (Label name') = getPos name'

instance HasPosition ModName where
  getPos (ModName _ _ range') = range'

instance HasPosition (Qualified k) where
  getPos (Qualified _ _ _ range') = range'

instance HasPosition k => HasPosition (Path k) where
  getPos (Local _ name) = getPos name
  getPos (Full _ name) = getPos name

instance SetPosition Ident where
  setPos r (Ident h k _ t) = Ident h k r t

instance SetPosition (Name k) where
  setPos r (Name h k n i) = Name h k n (setPos r i)

instance SetPosition ModName where
  setPos r (ModName h k _) = ModName h k r

instance SetPosition (Qualified k) where
  setPos r (Qualified h k i _) = Qualified h k i r

instance SetPosition Label where
  setPos r (Label n) = Label (setPos r n)

instance SetPosition k => SetPosition (Path k) where
  setPos r (Local h i) = Local h (setPos r i)
  setPos r (Full h i)  = Full h (setPos r i)
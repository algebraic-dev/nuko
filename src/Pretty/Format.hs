module Pretty.Format (
  Format(..),
  formatOr,
  formatAnd
) where

import Data.Text qualified as Text
import Relude

-- | A type class to beautifully represent some types of data
-- as Show should always contain code that can run
class Format a where
  format :: a -> Text

instance Format Int where format = show
instance Format Text where format = id

instance Format k => Format [k] where
  format k = Text.intercalate ", " (format <$> k)

instance (Format a, Format b) => Format (a,b) where format (a,b) = "(" <> format a <> ", " <> format b <> ")"

formatWith :: Format a => NonEmpty a -> Text -> Text
formatWith (ne :| []) _ = format ne
formatWith ne t =
  let formated = fmap format ne
      (rest, lastOne) = (init formated, last formated)
  in Text.intercalate ", " rest <> " " <> t <> " " <> lastOne

formatOr :: Format a => NonEmpty a -> Text
formatOr ne = formatWith ne "or"

formatAnd :: Format a => NonEmpty a -> Text
formatAnd ne = formatWith ne "and"

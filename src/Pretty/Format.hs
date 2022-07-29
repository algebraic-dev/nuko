module Pretty.Format (
  Format(..),
  formatOr
) where

import Relude.String (show)
import Data.Text (Text, intercalate)
import Data.Int (Int)
import Relude.List.NonEmpty (NonEmpty ((:|)), last, init)
import Relude.Monoid ((<>))
import Relude.Functor (Functor(fmap), (<$>))
import qualified Data.Text as Text

-- | A type class to beautifully represent some types of data
-- as Show should always contain code that can run
class Format a where
  format :: a -> Text

instance Format Int where format = show
instance Format Text where format = show

instance Format k => Format [k] where
  format k = Text.intercalate ", " (format <$> k)

instance (Format a, Format b) => Format (a,b) where format (a,b) = "(" <> format a <> ", " <> format b <> ")"

formatOr :: Format a => NonEmpty a -> Text
formatOr (ne :| []) = format ne
formatOr ne =
  let formated = fmap format ne
      (rest, lastOne) = (init formated, last formated)
  in intercalate ", " rest <> " or " <> lastOne

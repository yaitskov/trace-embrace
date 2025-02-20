-- | More detailed Show for debugging
-- E.g. show instance of lazy ByteString hides how many
-- chunks in the string.
module Debug.TraceIf.Show where

import Data.ByteString.Lazy.Internal qualified as L
import Data.ByteString.Internal (ByteString(..))

-- | Wrap value which has an opaque 'Show' instance.
newtype ShowTrace a = ShowTrace { unShowTrace :: a } deriving (Eq)

-- | Show 'ByteString' structure.
--
-- >>> showLbsAsIs ("a" <> "b")
showLbsAsIs :: L.ByteString -> [ByteString]
showLbsAsIs L.Empty = []
showLbsAsIs (L.Chunk x xs) = x : showLbsAsIs xs

instance Show (ShowTrace L.ByteString) where
  show = show . showLbsAsIs . unShowTrace

instance Show (ShowTrace ByteString) where
  show (ShowTrace bs@(BS fp len)) = "BS " <> show fp <> " " <> show len <> ":" <> show bs

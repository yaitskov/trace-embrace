-- | More detailed Show for debugging
-- E.g. show instance of lazy ByteString hides how many
-- chunks in the
module Debug.TraceIf.Show where

import Data.ByteString.Lazy.Internal qualified as L
import Data.ByteString.Internal (ByteString(..))

newtype ShowTrace a = ShowTrace { unShowTrace :: a } deriving (Eq)

showLbsAsIs :: L.ByteString -> [ByteString]
showLbsAsIs L.Empty = []
showLbsAsIs (L.Chunk x xs) = x : showLbsAsIs xs

instance Show (ShowTrace L.ByteString) where
  show = show . showLbsAsIs . unShowTrace

instance Show (ShowTrace ByteString) where
  show (ShowTrace bs@(BS fp len)) = "BS " <> show fp <> " " <> show len <> ":" <> show bs

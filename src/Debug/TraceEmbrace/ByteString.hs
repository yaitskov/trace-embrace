{-# OPTIONS_GHC -Wno-orphans #-}
module Debug.TraceEmbrace.ByteString where

import Data.ByteString.Lazy.Internal qualified as L
import Data.ByteString.Internal (ByteString(..))
import Debug.TraceEmbrace.Show
import Prelude hiding (Show (..))
import Prelude qualified as P

-- | Show 'ByteString' structure.
--
-- >>> showLbsAsIs ("a" <> "b")
showLbsAsIs :: L.ByteString -> [ByteString]
showLbsAsIs L.Empty = []
showLbsAsIs (L.Chunk x xs) = x : showLbsAsIs xs

instance {-# OVERLAPPING #-} Show (ShowTrace L.ByteString) where
  show = P.show . showLbsAsIs . unShowTrace

instance {-# OVERLAPPING #-} Show (ShowTrace ByteString) where
  show (ShowTrace bs@(BS fp len)) = "BS " <> P.show fp <> " " <> P.show len <> ":" <> P.show bs

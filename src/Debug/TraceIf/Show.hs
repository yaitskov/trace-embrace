{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}

-- | More detailed Show for debugging
-- E.g. show instance of lazy ByteString hides how many
-- chunks in the string.
module Debug.TraceIf.Show where

import Data.ByteString.Lazy.Internal qualified as L
import Data.ByteString.Internal (ByteString(..))
import GHC.Exts
import Prelude hiding (Show (..))
import Prelude qualified as P

-- | Levity polymorphic version 'P.Show'.
class Show (t :: TYPE r) where
  show :: t -> String

-- | Wrap value which has opaque 'Show' instance.
newtype ShowTrace a = ShowTrace { unShowTrace :: a }

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

instance Show Int# where
  show i# = P.show (I# i#) <> "#"

instance Show Float# where
  show i# = P.show (F# i#) <> "#"

instance Show Char# where
  show i# = P.show (C# i#) <> "#"

instance Show Word# where
  show i# = P.show (W# i#) <> "#"

instance Show Double# where
  show i# = P.show (D# i#) <> "#"

instance P.Show a => Show a where
  show = P.show

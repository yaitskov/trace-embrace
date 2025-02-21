{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | More detailed Show for debugging
-- E.g. show instance of lazy ByteString hides how many
-- chunks in the string.
module Debug.TraceIf.Show (module STh, ShowTrace (..)) where

import Data.ByteString.Lazy.Internal qualified as L
import Data.ByteString.Internal (ByteString(..))
import Debug.TraceIf.ShowTh as STh
import GHC.Exts
import Prelude hiding (Show (..))
import Prelude qualified as P


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

instance Show Addr# where
  show i# = P.show (Ptr @() i#) <> "#"

instance (Show a#) => Show (# a# #) where
  show (# a# #) = "(# " <> show a# <> " #)"

$(let utypes = [''Int#, ''Char#, ''Double#, ''Float#, ''Addr#]
   in concat <$> sequence [ show1 ut | ut <- utypes ])

instance (Show a#, Show b#) => Show (# a#, b# #) where
  show (# a#, b# #) = "(# " <> show a# <> ", " <> show b# <> " #)"

$(let utypes = [''Int#, ''Char#, ''Double#, ''Float#, ''Addr#]
   in concat <$> sequence [ show2 ut ut' | ut <- utypes, ut' <- utypes ])

instance (Show a, Show b, Show c) => Show (# a, b, c #) where
  show (# a, b, c #) = "(# " <> show a <> ", " <> show b <> ", " <> show c <> " #)"

$(let utypes = [''Int#, ''Char#, ''Double#, ''Float#, ''Addr#]
   in concat <$> sequence [ show3 ut ut' ut'' | ut <- utypes, ut' <- utypes, ut'' <- utypes ])

instance P.Show a => Show a where
  show = P.show

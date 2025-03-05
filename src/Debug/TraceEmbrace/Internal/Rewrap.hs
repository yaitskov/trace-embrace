{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}

module Debug.TraceEmbrace.Internal.Rewrap where

import GHC.Exts

-- | Unlifted value wrapper to be squize throughout trace function
class Rewrap (t :: TYPE r) b | t -> b where
  wrap :: t -> b
  unwrap :: b -> t

instance Rewrap Int# Int where
  wrap = I#
  unwrap (I# x#) = x#

instance Rewrap Double# Double where
  wrap = D#
  unwrap (D# x#) = x#

instance Rewrap Float# Float where
  wrap = F#
  unwrap (F# x#) = x#

instance Rewrap Addr# (Ptr ()) where
  wrap = Ptr
  unwrap (Ptr x#) = x#

instance Rewrap Char# Char where
  wrap = C#
  unwrap (C# x#) = x#

instance Rewrap Word# Word where
  wrap = W#
  unwrap (W# x#) = x#

instance Rewrap a a where
  wrap = id
  unwrap = id

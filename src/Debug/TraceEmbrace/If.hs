{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | "Debug.Trace" functions protected with environment variable.
module Debug.TraceEmbrace.If where

import GHC.Exts
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafeDupablePerformIO)

-- | Return 'True' if \"NOTRACE\" environment variable is not defined.
isTracingEnabled :: Bool
isTracingEnabled = unsafeDupablePerformIO (lookupEnv "NOTRACE") == Nothing

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

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}

module Debug.TraceEmbrace.Test.TraceEmbrace.TH.Format.Unboxed where

import Control.Lens
import Data.Generics.Labels ()
import Debug.TraceEmbrace
import Debug.TraceEmbrace.Test.TraceEmbrace.Config
import GHC.Exts
import Test.Tasty.HUnit ((@=?))

unit_unboxed_int :: IO ()
unit_unboxed_int = "foo; x#: 1#" @=? foo one
  where
    foo (I# x#) = $(trConstMsg "foo/(I# x#)")

unit_ret_unboxed_int :: IO ()
unit_ret_unboxed_int = "foo; x#: 1# => 1#" @=? foo one
  where
    foo (I# x#) = $(trFunMsg "foo/(I# x#)") x#

unit_traceWith_unboxed_int :: IO ()
unit_traceWith_unboxed_int = one @=? foo one
  where
    foo (I# x#) = (I# ($(tw "foo/(I# x#)") x#))

unit_trace_ret_unboxed_int :: IO ()
unit_trace_ret_unboxed_int = one @=? foo one
  where
    foo (I# x#) = (I# ($(tr "foo/(I# x#)") x#))

unit_unboxed_tuple :: IO ()
unit_unboxed_tuple = expec @=? foo (# 1#, 2# #)
  where
    expec :: String = "foo; t: (# 1#, 2# #) => 1#"
    foo :: (# Int#, Int# #) -> String
    foo t@(# x#, _ #) = $(trFunMsg "foo/t") x#

prop_ret_unboxed_tuple :: Bool
prop_ret_unboxed_tuple = isTrue# (1# ==# foo t)
  where
    t = (# 1#, 2# #)
    foo :: (# Int#, Int# #) -> Int#
    foo tt@(# x#, _ #) =
      $(setConfig (thresholdConfig & #mode .~ (TraceUnsafeIo StdErrSink)) >> tw "foo/tt") x#

prop_ret_unboxed_sum :: Bool
prop_ret_unboxed_sum = isTrue# $ 1# ==# foo (# 1# | #)
  where
    foo :: (# Int# | Double# #) -> Int#
    foo tt@(# x# | #) =
      $(setConfig (thresholdConfig & #mode .~ (TraceUnsafeIo StdErrSink)) >> tw "foo/tt") x#
    foo tt@(# | _ #) =
      $(setConfig (thresholdConfig & #mode .~ (TraceUnsafeIo StdErrSink)) >> tw "foo/tt") 0#

unit_ret_unboxed_unit :: IO ()
unit_ret_unboxed_unit = "foo; t: (# #)" @=? foo (#  #)
  where
    foo :: (# #) -> String
    foo t = $(trConstMsg "foo/t")

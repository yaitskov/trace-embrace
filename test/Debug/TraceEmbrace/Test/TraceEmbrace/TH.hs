{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module Debug.TraceEmbrace.Test.TraceEmbrace.TH where

import Data.ByteString.Lazy
import Debug.TraceEmbrace
import Debug.TraceEmbrace.Test.TraceEmbrace.Config
import Language.Haskell.TH.Lock
import Language.Haskell.TH.Syntax
import Test.Tasty.HUnit ((@=?))

ensureSerialCompilationVerbose

sum4 :: (Show a, Num a) => a -> a -> a -> a -> a
sum4 $a $a $s_ $a | $tg = $u
sum4 a1 a2 a3 a4 = a1 + a2 + a3 + a4

unit_sum4_a_tg_u :: IO ()
unit_sum4_a_tg_u = withPrefixEnvVar thresholdConfig "" $ sum4 one one one one @=? 4

unit_tr_trace :: IO ()
unit_tr_trace = withPrefixEnvVar thresholdConfig "" $ go one
  where
    go x = x @=? foo x
      where
        foo = $(tr "-foo trace/x")

unit_tr_twice_on_same_line :: IO ()
unit_tr_twice_on_same_line = withPrefixEnvVar thresholdConfig "" $ go one
  where
    go x = x @=? foo x
      where
        foo = $(tr "-foo trace/x") . $(tr "-foo trace/x")

unit_tr_info :: IO ()
unit_tr_info = withPrefixEnvVar thresholdConfig "" $ go one
  where
    go x = x @=? foo x
      where
        foo = $(tr "foo info/x")

unit_tr_warning :: IO ()
unit_tr_warning = withPrefixEnvVar thresholdConfig "" $ go one
  where
    go x = x @=? foo x
      where
        foo = $(tr "!foo warning/x")

unit_tr_error :: IO ()
unit_tr_error = withPrefixEnvVar thresholdConfig "" $ go one
  where
    go x = x @=? foo x
      where
        foo = $(tr "|foo error/x")

unit_tw :: IO ()
unit_tw = withPrefixEnvVar thresholdConfig "" $ go one
  where
    go x = x @=? foo x
    foo x = $(tw "tw foo/x") x

unit_tw' :: IO ()
unit_tw' = withPrefixEnvVar thresholdConfig "" $ go bs
  where
    bs :: ByteString = "x" <> "y"
    go x = x @=? foo x
    foo x = $(tw' "tw foo/x") x

unit_trIo :: IO ()
unit_trIo = withPrefixEnvVar thresholdConfig "" $ go one
  where
    go x = (x @=?) =<< foo x
      where
        foo y = $(trIo "foo info/y") >> pure y

unit_ensure_mode_trace_std :: IO ()
unit_ensure_mode_trace_std =
  $( lift =<< ((.mode) <$> getConfig)  ) @=? TraceStd

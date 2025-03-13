{-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module Debug.TraceEmbrace.Test.TraceEmbrace.TH where

import Data.ByteString.Lazy
import Debug.TraceEmbrace
import Debug.TraceEmbrace.Test.TraceEmbrace.Config
import Test.Tasty.HUnit ((@=?))

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

unit_trIo_ :: IO ()
unit_trIo_ = withPrefixEnvVar thresholdConfig "" $ go one
  where
    go x = (x @=?) =<< foo x
      where
        foo y = $(trIo "foo info/y") >> pure y

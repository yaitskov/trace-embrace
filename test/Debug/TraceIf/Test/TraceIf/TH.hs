-- {-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module Debug.TraceIf.Test.TraceIf.TH where

import Data.ByteString.Lazy
import Debug.TraceIf
import Debug.TraceIf.Test.TraceIf.Config
import Test.Tasty.HUnit ((@=?))

unit_tr_trace :: IO ()
unit_tr_trace = go one
  where
    go x = x @=? foo x
      where
        foo = $(tr "-foo trace/x")

unit_tr_info :: IO ()
unit_tr_info = go one
  where
    go x = x @=? foo x
      where
        foo = $(tr "foo info/x")

unit_tr_warning :: IO ()
unit_tr_warning = go one
  where
    go x = x @=? foo x
      where
        foo = $(tr "!foo warning/x")

unit_tr_error :: IO ()
unit_tr_error = go one
  where
    go x = x @=? foo x
      where
        foo = $(tr "|foo error/x")

unit_tw :: IO ()
unit_tw = go one
  where
    go x = x @=? foo x
    foo x = $(tw "tw foo/x") x

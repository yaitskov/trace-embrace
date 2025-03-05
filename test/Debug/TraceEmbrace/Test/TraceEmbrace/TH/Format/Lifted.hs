{-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module Debug.TraceEmbrace.Test.TraceEmbrace.TH.Format.Lifted where

import Data.ByteString.Lazy
import Debug.TraceEmbrace.ByteString ()
import Debug.TraceEmbrace.Internal.TH
import Debug.TraceEmbrace.Test.TraceEmbrace.Config
import Test.Tasty.HUnit ((@=?))

-- not working: https://gitlab.haskell.org/ghc/ghc/-/issues/25775
default (Int)

unit_traceMessage_const :: IO ()
unit_traceMessage_const =
  ("Debug.TraceEmbrace.Test.TraceEmbrace.TH.Format.Lifted::unit_traceMessage_const" :: String) @=?
     $(traceMessage (TrMsgAndVars "skipped") positionOnly svars)

unit_traceMessage_fun :: IO ()
unit_traceMessage_fun = ("foo => True" :: String) @=?  $(trFunMsg "foo") True

unit_svarsWith :: IO ()
unit_svarsWith =
  let x = one in
    "bye; x: 1 => 2" @=? $(trFunMsg "bye/x") two

unit_let_x :: IO ()
unit_let_x = "bye; one: 1" @=? $(trConstMsg "bye/one")

unit_where_x :: IO ()
unit_where_x = "bye; x: 1" @=? $(trConstMsg "bye/x")
  where
    x = one

unit_string :: IO ()
unit_string = "bye; x: \"abc\"" @=? $(trConstMsg "bye/x")
  where
    x = "abc" :: String

unit_show_trace :: IO ()
unit_show_trace = "; x: [\"ab\",\"c\"]" @=? $(trConstMsg "/;x")
  where
    x = ("ab" <> "c") :: ByteString

unit_show_and_show_trace :: IO ()
unit_show_and_show_trace = "; y: True; x: [\"ab\",\"c\"]" @=? $(trConstMsg "/y;x")
  where
    x = ("ab" <> "c") :: ByteString
    y = True

unit_show_2_vars :: IO ()
unit_show_2_vars = "; y: True; x: \"abc\"" @=? $(trConstMsg "/y x")
  where
    x = ("ab" <> "c") :: ByteString
    y = True

unit_ignore_parenthesis_and_undebar :: IO ()
unit_ignore_parenthesis_and_undebar = "foo; x: 1" @=? foo (one, two)
  where
    foo (x, _) = $(trConstMsg "foo/(x, _)")

unit_empty_var_list :: IO ()
unit_empty_var_list = "hello" @=? $(trConstMsg "hello/")

unit_ignore_single_line_comment :: IO ()
unit_ignore_single_line_comment = "foo; x: 1" @=? foo one
  where
    foo x -- hello
      = $(trConstMsg "foo/x -- hello")

unit_ignore_string :: IO ()
unit_ignore_string = "foo; x: 1" @=? foo ("y y" :: String) one ("z" :: String)
  where
    foo "y y" x "z" = $(trConstMsg "foo/\"y y\" x \"z\" =")
    foo a _ c = "dead code due: " <> show (a, c)

unit_ignore_comment :: IO ()
unit_ignore_comment = "foo; x: 1" @=? foo one
  where
    foo {- comment before -} x {-after-} =
      $(trConstMsg "foo/{- comment before -} x {-after-}")

unit_ignore_bang :: IO ()
unit_ignore_bang = "foo; x: 1" @=? foo one
  where
    foo !x = $(trConstMsg "foo/!x")

unit_at :: IO ()
unit_at = "foo; x: Just 1; y: 1" @=? foo (Just one)
  where
    foo x@(Just y) = $(trConstMsg "foo/x@(Just y)")
    foo r = fail $ show r

data Point = Point { xPoint :: Int, yPoint :: Int } deriving Show

unit_record :: IO ()
unit_record = "foo; xPoint: 12" @=? foo (Point one two)
  where
    foo Point {xPoint,..} = $(trConstMsg "foo/Point {xPoint,..}") <> show yPoint

unit_record2 :: IO ()
unit_record2 = "foo; xPoint: 12" @=? foo (Point one two)
  where
    foo Point { xPoint, ..} = $(trConstMsg "foo/Point { xPoint, ..}") <> show yPoint

unit_list_int :: IO ()
unit_list_int = "foo; a: 1" @=? foo [one, two, 123]
  where
    foo [a, _b, 123] = $(trConstMsg "foo/[a, _b, 123]")
    foo r = fail $ show r

unit_char :: IO ()
unit_char = "foo; l: \"b\"" @=? foo "ab"
  where
    foo ('a':l) = $(trConstMsg "foo/('a':l)")
    foo r = fail $ show r

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
-- {-# OPTIONS_GHC -ddump-splices #-}

module Debug.TraceIf.Test.TraceIf.TH where

import Data.ByteString.Lazy
import Debug.TraceIf
import Test.Tasty.HUnit

default (Int)


unit_traceMessage :: IO ()
unit_traceMessage =
  (" 20:Debug.TraceIf.Test.TraceIf.TH " :: String) @=? $(traceMessage)

unit_svarsWith :: IO ()
unit_svarsWith =
  let x = 123 in
    "bye; x: 123 => 9" @=? $(svarsWith "bye/x") 9

unit_let_x :: IO ()
unit_let_x =
  let x = 123 in
    "bye; x: 123" @=? $(svars "bye/x")

unit_where_x :: IO ()
unit_where_x = "bye; x: 1234" @=? $(svars "bye/x")
  where
    x = 1234

unit_string :: IO ()
unit_string = "bye; x: \"abc\"" @=? $(svars "bye/x")
  where
    x = "abc" :: String

unit_show_trace :: IO ()
unit_show_trace = "; x: [\"ab\",\"c\"]" @=? $(svars "/#x")
  where
    x = ("ab" <> "c") :: ByteString

unit_show_and_show_trace :: IO ()
unit_show_and_show_trace = "; y: True; x: [\"ab\",\"c\"]" @=? $(svars "/y#x")
  where
    x = ("ab" <> "c") :: ByteString
    y = True

unit_show_2_vars :: IO ()
unit_show_2_vars = "; y: True; x: \"abc\"" @=? $(svars "/y x")
  where
    x = ("ab" <> "c") :: ByteString
    y = True

unit_tr :: IO ()
unit_tr = go 23
  where
    go x = x @=? foo x
      where
        foo = $(tr "foo get/x")

unit_tw :: IO ()
unit_tw = go 12
  where
    go x = x @=? foo x
    foo x = $(tw "foo get/x") x

unit_ignore_parenthesis_and_undebar :: IO ()
unit_ignore_parenthesis_and_undebar = "foo; x: 1" @=? foo (1, 2)
  where
    foo (x, _) = $(svars "foo/(x, _)")

unit_empty_var_list :: IO ()
unit_empty_var_list = "hello" @=? $(svars "hello/")

unit_ignore_single_line_comment :: IO ()
unit_ignore_single_line_comment = "foo; x: 1" @=? foo 1
  where
    foo x -- hello
      = $(svars "foo/x -- hello")

unit_ignore_string :: IO ()
unit_ignore_string = "foo; x: 1" @=? foo ("y y" :: String) 1 ("z" :: String)
  where
    foo "y y" x "z" = $(svars "foo/\"y y\" x \"z\" =")
    foo a _ c = "dead code due: " <> show (a, c)

unit_ignore_comment :: IO ()
unit_ignore_comment = "foo; x: 1" @=? foo 1
  where
    foo {- comment before -} x {-after-} =
      $(svars "foo/{- comment before -} x {-after-}")

unit_ignore_bang :: IO ()
unit_ignore_bang = "foo; x: 1" @=? foo 1
  where
    foo !x = $(svars "foo/!x")

unit_at :: IO ()
unit_at = "foo; x: Just 1; y: 1" @=? foo (Just 1)
  where
    foo x@(Just y) = $(svars "foo/x@(Just y)")
    foo r = fail $ show r

data Point = Point { xPoint :: Int, yPoint :: Int } deriving Show

unit_record :: IO ()
unit_record = "foo; xPoint: 12" @=? foo (Point 1 2)
  where
    foo Point {xPoint,..} = $(svars "foo/Point {xPoint,..}") <> show yPoint

unit_record2 :: IO ()
unit_record2 = "foo; xPoint: 12" @=? foo (Point 1 2)
  where
    foo Point { xPoint, ..} = $(svars "foo/Point { xPoint, ..}") <> show yPoint

unit_list_int :: IO ()
unit_list_int = "foo; a: 1" @=? foo [1, 2, 123]
  where
    foo [a, _b, 123] = $(svars "foo/[a, _b, 123]")
    foo r = fail $ show r

unit_char :: IO ()
unit_char = "foo; l: \"b\"" @=? foo "ab"
  where
    foo ('a':l) = $(svars "foo/('a':l)")
    foo r = fail $ show r

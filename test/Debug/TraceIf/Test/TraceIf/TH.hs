{-# LANGUAGE TemplateHaskell #-}

module Debug.TraceIf.Test.TraceIf.TH where

import Debug.TraceIf
import Test.Tasty.HUnit

unit_empty_var_list :: IO ()
unit_empty_var_list =
  " 10:Debug.TraceIf.Test.TraceIf.TH hello"  @=? $(svars "hello/")

unit_let_x :: IO ()
unit_let_x =
  let x = 123 :: Int in
    " 15:Debug.TraceIf.Test.TraceIf.TH bye; x: 123"  @=? $(svars "bye/x")

unit_where_x :: IO ()
unit_where_x =
  " 19:Debug.TraceIf.Test.TraceIf.TH bye; x: 1234"  @=? $(svars "bye/x")
  where
    x = 1234 :: Int

unit_string :: IO ()
unit_string =
  " 25:Debug.TraceIf.Test.TraceIf.TH bye; x: \"abc\""  @=? $(svars "bye/x")
  where
    x = "abc" :: String

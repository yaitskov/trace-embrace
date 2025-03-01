{-# LANGUAGE OverloadedStrings #-}
module Debug.TraceEmbrace.Test.TraceEmbrace.FileIndex where

import Data.IntMap.Strict qualified as IM
import Debug.TraceEmbrace.Test.TraceEmbrace.DemoIndex
import Test.Tasty.HUnit ((@=?))

unit_file_index :: IO ()
unit_file_index = IM.fromList l @=? demoIndex
  where
    l = [(7,"foo"),(12,"***"),(17,"show"),(23,"mylen"),(26,"+++"),(27,"mylen")]

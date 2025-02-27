{-# LANGUAGE OverloadedStrings #-}
module Debug.TraceIf.Test.TraceIf.FileIndex where

import Data.IntMap.Strict qualified as IM
import Debug.TraceIf.Test.TraceIf.DemoIndex
import Test.Tasty.HUnit ((@=?))

unit_file_index :: IO ()
unit_file_index = IM.fromList l @=? demoIndex
  where
    l = [(7,"foo"),(12,"***"),(17,"show"),(23,"mylen"),(26,"+++"),(27,"mylen")]

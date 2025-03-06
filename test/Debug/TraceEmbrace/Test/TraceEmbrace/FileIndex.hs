{-# LANGUAGE OverloadedStrings #-}
module Debug.TraceEmbrace.Test.TraceEmbrace.FileIndex where

import Data.IntMap.Strict qualified as IM
import Debug.TraceEmbrace.Test.TraceEmbrace.DemoIndex
import Test.Tasty.HUnit ((@=?))

unit_fileindex :: IO ()
unit_fileindex = IM.fromList l @=? demoIndex
  where
    l = [(12,"***"),(17,"show"),(23,"mylen"),(26,"+++"),(27,"mylen"),(30,"foo")]

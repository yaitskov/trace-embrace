{-# LANGUAGE TemplateHaskell #-}
module Debug.TraceIf.Test.TraceIf.DemoIndex where

import Data.IntMap.Strict qualified as IM
import Debug.TraceIf.FileIndex
import Language.Haskell.TH.Syntax

demoIndex :: IM.IntMap FunName
demoIndex = $(lift =<< getLineFileIndex' =<< (<> "/test/Demo.hs") <$> qGetPackageRoot)

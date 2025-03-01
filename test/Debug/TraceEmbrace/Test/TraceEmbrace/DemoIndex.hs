module Debug.TraceEmbrace.Test.TraceEmbrace.DemoIndex where

import Data.IntMap.Strict qualified as IM
import Debug.TraceEmbrace.FileIndex
import Language.Haskell.TH.Syntax

demoIndex :: IM.IntMap FunName
demoIndex = $(lift =<< getLineFileIndex' =<< (<> "/test/Demo.hs") <$> qGetPackageRoot)

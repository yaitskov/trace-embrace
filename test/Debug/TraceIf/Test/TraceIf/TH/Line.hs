-- | line number often changes which breaks test
-- So the test is isolated from others
module Debug.TraceIf.Test.TraceIf.TH.Line where

import Debug.TraceIf
import Debug.TraceIf.Test.TraceIf.Config
import Test.Tasty.HUnit ((@=?))

unit_traceMessage_line :: IO ()
unit_traceMessage_line =
  ("11:" :: String) @=? $(traceMessage (TrMsgAndVars "skipped") lineOnly svars)

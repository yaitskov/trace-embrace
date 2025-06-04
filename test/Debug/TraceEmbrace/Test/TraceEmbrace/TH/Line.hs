-- | line number often changes which breaks test
-- So the test is isolated from others
module Debug.TraceEmbrace.Test.TraceEmbrace.TH.Line where

import Debug.TraceEmbrace.Internal.TH
import Debug.TraceEmbrace.Test.TraceEmbrace.Config
import Test.Tasty.HUnit ((@=?))

unit_traceMessage_line :: IO ()
unit_traceMessage_line =
  ("11:" :: String) @=? $(traceMessage (TrMsgAndVars [] "skipped") lineOnly svars)

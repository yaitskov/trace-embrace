module Debug.TraceIf.Test.TraceIf.Config where

import Debug.TraceIf.Config hiding (traceMessage)
import Debug.TraceIf.TH
import Language.Haskell.TH.Syntax

positionOnly :: TraceMessageFormat
positionOnly =
  defaultTraceMessageFormat
  { traceLinePattern =
    [ FullyQualifiedModule
    , Delimiter "::"
    , FunctionName
    ]
  }

lineOnly :: TraceMessageFormat
lineOnly =
  defaultTraceMessageFormat
  { traceLinePattern =
    [ LineNumber
    , Delimiter ":"
    ]
  }

msgAndVarsOnly :: TraceMessageFormat
msgAndVarsOnly =
  defaultTraceMessageFormat { traceLinePattern = [ LiteralMessage, Variables ] }

trConstMsg :: String -> Q Exp
trConstMsg msgAndVars = traceMessage (TrMsgAndVars msgAndVars) msgAndVarsOnly svars

trFunMsg :: String -> Q Exp
trFunMsg msgAndVars = traceMessage (TrMsgAndVars msgAndVars) msgAndVarsOnly svarsWith

one :: Int
one = 1

two :: Int
two = 2

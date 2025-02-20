-- | "Debug.Trace" functions protected with environment variable.
module Debug.TraceIf.If where

import Debug.Trace qualified as T
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafeDupablePerformIO)

-- | Return 'True' if \"NOTRACE\" environment variable is not defined.
isTracingEnabled :: Bool
isTracingEnabled = unsafeDupablePerformIO (lookupEnv "NOTRACE") == Nothing

-- | Wrapper around 'T.trace'.
-- It is not called if \"NOTRACE\" environment variable is defined.
trace :: String -> a -> a
trace
  | isTracingEnabled = T.trace
  | otherwise = flip const

-- | Wrapper around 'T.traceWith'.
-- It is not called if \"NOTRACE\" environment variable is defined.
traceWith :: (a -> String) -> a -> a
traceWith
  | isTracingEnabled = T.traceWith
  | otherwise = flip const

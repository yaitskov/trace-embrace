module Debug.TraceIf.If where

import Debug.Trace qualified as T
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafeDupablePerformIO)

isTracingEnabled :: Bool
isTracingEnabled = unsafeDupablePerformIO (lookupEnv "NOTRACE") == Nothing

trace :: String -> a -> a
trace
  | isTracingEnabled = T.trace
  | otherwise = flip const

traceWith :: (a -> String) -> a -> a
traceWith
  | isTracingEnabled = T.traceWith
  | otherwise = flip const

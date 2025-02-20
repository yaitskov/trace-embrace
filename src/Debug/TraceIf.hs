module Debug.TraceIf (trace, traceWith, module TT) where

import Debug.Trace qualified as T
import Debug.TraceIf.TH as TT
import Debug.TraceIf.Show as TT
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

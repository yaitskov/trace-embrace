-- | The package is assumed to be used with TH, but if you need just
-- a pluggable "Debug.Trace" then import "Debug.TraceIf.If"
-- instead of "Debug.TraceIf".
module Debug.TraceIf (module TT) where

import Debug.TraceIf.Show as TT
import Debug.TraceIf.TH as TT

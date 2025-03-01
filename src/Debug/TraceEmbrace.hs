-- | The package is assumed to be used with TH.
module Debug.TraceEmbrace (module TT) where

import Debug.TraceEmbrace.Config as TT
import Debug.TraceEmbrace.Show as TT hiding (Show (..))
import Debug.TraceEmbrace.TH as TT

-- | The package is assumed to be used with TH.
module Debug.TraceIf (module TT) where

import Debug.TraceIf.Show as TT hiding (Show (..))
import Debug.TraceIf.TH as TT

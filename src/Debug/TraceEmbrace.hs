-- | The root module to be imported by applications and libraries.
module Debug.TraceEmbrace (ShowTrace (..), module TT) where

import Debug.TraceEmbrace.Config as TT
import Debug.TraceEmbrace.ByteString (ShowTrace (..))
import Debug.TraceEmbrace.TH as TT

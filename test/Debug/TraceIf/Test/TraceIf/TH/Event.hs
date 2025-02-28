module Debug.TraceIf.Test.TraceIf.TH.Event where

import Control.Lens
import Debug.TraceIf.Config
import Debug.TraceIf.Internal.TH
import Debug.TraceIf.Test.TraceIf.Config
import Test.Tasty.HUnit ((@=?))

unit_event_mode :: IO ()
unit_event_mode =
  withPrefixEnvVar thresholdConfig "" $ do
    one @=? $(setConfig (thresholdConfig & #mode .~ TraceEvent)
              >> tr poisonedId "tm") one

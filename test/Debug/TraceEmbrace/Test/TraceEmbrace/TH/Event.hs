module Debug.TraceEmbrace.Test.TraceEmbrace.TH.Event where

import Control.Lens
import Debug.TraceEmbrace.Config.Type.EnvVar
import Debug.TraceEmbrace.Internal.TH
import Debug.TraceEmbrace.Test.TraceEmbrace.Config
import Test.Tasty.HUnit ((@=?))

unit_event_mode :: IO ()
unit_event_mode =
  withPrefixEnvVar thresholdConfig "" $ do
    one @=? $(setConfig (thresholdConfig & #mode .~ TraceEvent)
              >> tr poisonedId "tm") one

unit_event_trIo :: IO ()
unit_event_trIo =  withPrefixEnvVar thresholdConfig "" $ go one
  where
    go x = (x @=?) =<< foo x
      where
        foo y = $(setConfig (thresholdConfig & #mode .~ TraceEvent)
                  >> trIo [| pure () |] "foo/y") >> pure y

unit_marker_io :: IO ()
unit_marker_io =  withPrefixEnvVar thresholdConfig "" $ go one
  where
    go x = (x @=?) =<< foo x
      where
        foo y = $(trIoFunMarker [| pure () |]) >> pure y

unit_fun_marker :: IO ()
unit_fun_marker =  withPrefixEnvVar thresholdConfig "" $ go one
  where
    go x = x @=? foo x
      where
        foo = $(trFunMarker poisonedId)

module Debug.TraceEmbrace.Test.TraceEmbrace.TH.Event where

import Control.Exception
import Control.Lens
import Debug.TraceEmbrace.Config.Type
import Debug.TraceEmbrace.Internal.TH
import Debug.TraceEmbrace.Test.TraceEmbrace.Config
import Test.Tasty.HUnit ((@=?))
import System.Directory

unit_unsafeio_stderr_mode :: IO ()
unit_unsafeio_stderr_mode =
  withPrefixEnvVar thresholdConfig "" $ do
    one @=? $(setConfig (thresholdConfig & #mode .~ (TraceUnsafeIo StdErrSink))
              >> tr poisonedId "tm") one

unit_unsafeio_stdout_mode :: IO ()
unit_unsafeio_stdout_mode =
  withPrefixEnvVar thresholdConfig "" $ do
    one @=? $(setConfig (thresholdConfig & #mode .~ (TraceUnsafeIo StdOutSink))
              >> tr poisonedId "tm") one

unit_unsafeio_filesink_mode :: IO ()
unit_unsafeio_filesink_mode =
  withPrefixEnvVar thresholdConfig "" $ do
    one @=? $(setConfig (thresholdConfig & #mode .~ (TraceUnsafeIo $ FileSink fileSynkTmp))
              >> tr poisonedId "tm") one
    closeUnsafeIoSink
    finally
      (("Debug.TraceEmbrace.Test.TraceEmbrace.TH.Event::unit_unsafeio_filesink_mode: tm\n" @=?) =<< readFile fileSynkTmp)
      (removeFile fileSynkTmp)

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

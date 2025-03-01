{-# LANGUAGE OverloadedRecordDot #-}
module Debug.TraceEmbrace.Test.TraceEmbrace.TH.Threshold where

import Control.Lens
import Data.Generics.Labels ()
import Data.Yaml qualified as Y
import Debug.TraceEmbrace.Internal.TH qualified as I
import Debug.TraceEmbrace.Config
import Debug.TraceEmbrace.Test.TraceEmbrace.Config
import System.FilePath
import System.IO.Temp
import Test.Tasty.HUnit ((@=?))


unit_mode_disabled :: IO ()
unit_mode_disabled =
  withPrefixEnvVar thresholdConfig "" $ do
    two @=? $(setConfig (thresholdConfig & #mode .~ TraceDisabled)
              >> I.tr poisonedId "tm") one

unit_mode_enabled_with_evar_unset :: IO ()
unit_mode_enabled_with_evar_unset =
  withPrefixEnvVar thresholdConfig "" $ do
    one @=? $(setConfig (thresholdConfig & #mode .~ TraceStd)
              >> I.tr poisonedId "tm") one

unit_mode_enabled_with_evar_unset_trace_level :: IO ()
unit_mode_enabled_with_evar_unset_trace_level =
  withPrefixEnvVar thresholdConfig "" $ do
    two @=? $(setConfig (thresholdConfig & #mode .~ TraceStd)
              >> I.tr poisonedId "-tm") one

unit_mode_enabled_with_runtime_threshold_level_too_high :: IO ()
unit_mode_enabled_with_runtime_threshold_level_too_high =
  withSystemTempDirectory "trace-embrace" $ \d ->
    let fp = d </> "runtime-conf.yaml" in do
      Y.encodeFile fp (emptyPrefixTraceLevel Error)
      withPrefixEnvVar thresholdConfig fp $ do
        two @=? $(setConfig (thresholdConfig & #mode .~ TraceStd)
                  >> I.tr poisonedId "tm") one

unit_mode_enabled_with_runtime_threshold_level_ok :: IO ()
unit_mode_enabled_with_runtime_threshold_level_ok =
  withSystemTempDirectory "trace-embrace" $ \d ->
    let fp = d </> "runtime-conf.yaml" in do
      Y.encodeFile fp (emptyPrefixTraceLevel Error)
      withPrefixEnvVar thresholdConfig fp $ do
        one @=? $(setConfig (thresholdConfig & #mode .~ TraceStd)
                  >> I.tr poisonedId "|tm") one

unit_runtime_cannot_lower_threshold_level :: IO ()
unit_runtime_cannot_lower_threshold_level =
  withSystemTempDirectory "trace-embrace" $ \d ->
    let fp = d </> "runtime-conf.yaml" in do
      Y.encodeFile fp (emptyPrefixTraceLevel Trace)
      withPrefixEnvVar thresholdConfig fp $ do
        two @=? $(setConfig (thresholdConfig & #mode .~ TraceStd)
                  >> I.tr poisonedId "-tm") one

unit_mode_enabled_with_evar_set_empty :: IO ()
unit_mode_enabled_with_evar_set_empty = do
  withPrefixEnvVar thresholdConfig "-" $ do
    two @=? $(setConfig (thresholdConfig & #mode .~ TraceStd)
              >> I.tr [| \x -> x + 1 |] "tm") one

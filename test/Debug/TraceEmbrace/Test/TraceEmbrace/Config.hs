module Debug.TraceEmbrace.Test.TraceEmbrace.Config where

import Control.Concurrent.MVar
import Control.Exception
import Control.Lens
import Data.Cache.LRU
import Data.Generics.Labels ()
import Data.IORef
import Data.Maybe
import Debug.TraceEmbrace.Config
import Debug.TraceEmbrace.Config.Type.EnvVar
import Debug.TraceEmbrace.Config.Type.Level
import Debug.TraceEmbrace.Config.Type.TraceMessage
import Debug.TraceEmbrace.Internal.TH
import Language.Haskell.TH.Syntax
import Refined
import System.Environment

positionOnly :: TraceMessageFormat
positionOnly =
  defaultTraceMessageFormat
  { traceLinePattern = $$(refineTH
                          [ FullyQualifiedModule
                          , Delimiter "::"
                          , FunctionName
                          ])
                       :: Refined NonEmpty [TraceMessageElement]
  }

lineOnly :: TraceMessageFormat
lineOnly =
  defaultTraceMessageFormat
  { traceLinePattern = $$(refineTH
    [ LineNumber
    , Delimiter ":"
    ]) :: Refined NonEmpty [TraceMessageElement]
  }

msgAndVarsOnly :: TraceMessageFormat
msgAndVarsOnly =
  defaultTraceMessageFormat
  { traceLinePattern =
      $$(refineTH [ LiteralMessage, Variables ]) ::
      Refined NonEmpty [TraceMessageElement]
  }

trConstMsg :: String -> Q Exp
trConstMsg msgAndVars = traceMessage (TrMsgAndVars msgAndVars) msgAndVarsOnly svars

trFunMsg :: String -> Q Exp
trFunMsg msgAndVars = traceMessage (TrMsgAndVars msgAndVars) msgAndVarsOnly svarsWith

one :: Int
one = 1

two :: Int
two = 2

thresholdConfig :: TraceEmbraceConfig
thresholdConfig = v & #levels .~ mkPrefixTree (emptyPrefixTraceLevel Info)
 where
   v :: TraceEmbraceConfig
   v = case (yaml2Config <$> validateYamlConfig newYamlConfig) of
         Right r -> r
         Left e -> error e

setConfig :: TraceEmbraceConfig -> Q ()
setConfig c =
  runIO (atomicWriteIORef traceIfConfigRef (Just c))

withEnv :: String -> String -> IO a -> IO a
withEnv evar val a = do
  oldVal <- lookupEnv evar
  bracket (setEnv evar val)
          (\_ -> setEnv evar $ fromMaybe "" oldVal)
          (\_ -> a)

withPrefixEnvVar :: TraceEmbraceConfig -> String -> IO a -> IO a
withPrefixEnvVar c val a =
  case c ^. #runtimeLevelsOverrideEnvVar of
    Ignored -> fail "Env var is ignored"
    CapsPackageName ->
      go $ packageBasedEnvVarPrefix <> "TRACE_EMBRACE_0_0_2_INPLACE_TEST"
    EnvironmentVariable ev -> go ev
  where
    go ev =
      withEnv ev val $ do
        modifyMVar_ runtimeTraceEmbraceConfigRef (\_ -> pure . newLRU $ Just 7)
        a

poisonedId :: Q Exp
poisonedId = [| \x -> x + 1 |]

module Debug.TraceIf.Test.TraceIf.Config where

import Control.Concurrent.MVar
import Control.Exception
import Control.Lens
import Data.Generics.Labels ()
import Data.IORef
import Data.Maybe
import Debug.TraceIf.Config
import Debug.TraceIf.Internal.TH
import Language.Haskell.TH.Syntax
import System.Environment

positionOnly :: TraceMessageFormat
positionOnly =
  defaultTraceMessageFormat
  { traceLinePattern =
    [ FullyQualifiedModule
    , Delimiter "::"
    , FunctionName
    ]
  }

lineOnly :: TraceMessageFormat
lineOnly =
  defaultTraceMessageFormat
  { traceLinePattern =
    [ LineNumber
    , Delimiter ":"
    ]
  }

msgAndVarsOnly :: TraceMessageFormat
msgAndVarsOnly =
  defaultTraceMessageFormat { traceLinePattern = [ LiteralMessage, Variables ] }

trConstMsg :: String -> Q Exp
trConstMsg msgAndVars = traceMessage (TrMsgAndVars msgAndVars) msgAndVarsOnly svars

trFunMsg :: String -> Q Exp
trFunMsg msgAndVars = traceMessage (TrMsgAndVars msgAndVars) msgAndVarsOnly svarsWith

one :: Int
one = 1

two :: Int
two = 2

thresholdConfig :: TraceIfConfig
thresholdConfig =
  (fromJust (yaml2Config <$> validateYamlConfig newYamlConfig))
  & #levels .~ mkPrefixTree (emptyPrefixTraceLevel Info)

setConfig :: TraceIfConfig -> Q ()
setConfig c =
  runIO (atomicWriteIORef traceIfConfigRef (Just c))

withEnv :: String -> String -> IO a -> IO a
withEnv evar val a = do
  oldVal <- lookupEnv evar
  bracket (setEnv evar val)
          (\_ -> setEnv evar $ fromMaybe "" oldVal)
          (\_ -> a)

withPrefixEnvVar :: TraceIfConfig -> String -> IO a -> IO a
withPrefixEnvVar c val a =
  case c ^. #runtimeLevelsOverrideEnvVar of
    Ignored -> fail "Env var is ignored"
    EnvironmentVariable ev -> do
      withEnv ev val $ do
        modifyMVar_ runtimeTraceIfConfigRef (const $ pure Nothing)
        a

poisonedId :: Q Exp
poisonedId = [| \x -> x + 1 |]

module Debug.TraceEmbrace.Test.TraceEmbrace.Config where

import Control.Concurrent.MVar
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Cache.LRU
import Data.Generics.Labels ()
import Data.IORef
import Data.Maybe
import Debug.TraceEmbrace.Config
import Debug.TraceEmbrace.Internal.TH
import Language.Haskell.TH.Syntax
import Refined
import System.Directory
import System.Environment
import System.IO
import System.IO.Temp


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
trConstMsg msgAndVars = traceMessage (TrMsgAndVars [] msgAndVars) msgAndVarsOnly svars

trFunMsg :: String -> Q Exp
trFunMsg msgAndVars = traceMessage (TrMsgAndVars [] msgAndVars) msgAndVarsOnly svarsWith

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

-- | Always call 'resetConfig' at the end of module using 'setConfig'.
setConfig :: TraceEmbraceConfig -> Q ()
setConfig c =
  runIO (do closeUnsafeIoSink
            atomicWriteIORef traceEmbraceConfigRef (Just c))

-- | Every module using 'setConfig' should call at the end
-- 'resetConfig' because setConfig does not create real file.
resetConfig :: Q [Dec]
resetConfig = do
  runIO (do closeUnsafeIoSink
            atomicWriteIORef traceEmbraceConfigRef Nothing)
  pure []

closeUnsafeIoSink :: IO ()
closeUnsafeIoSink = do
  atomicModifyIORef' unsafeIoSink (\x -> (Nothing, x)) >>= \case
    Nothing -> pure ()
    Just h -> hClose h

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
      go $ packageBasedEnvVarPrefix <> "TRACE_EMBRACE"
    EnvironmentVariable ev -> go ev
  where
    go ev =
      withEnv ev val $ do
        modifyMVar_ runtimeTraceEmbraceConfigRef (\_ -> pure . newLRU $ Just 7)
        a

poisonedId :: Q Exp
poisonedId = [| \x -> x + 1 |]

fileSynkTmp :: FilePath
fileSynkTmp =
  $(LitE . StringL <$> (runIO (do
                                  f <- emptySystemTempFile "unsafeIoSinkFile.log"
                                  doesFileExist f >>= flip when (removeFile f)
                                  pure f)))

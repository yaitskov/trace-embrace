{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
module Debug.TraceIf.Config.Load where

import Control.Concurrent.MVar
import Control.Exception
import Data.Generics.Labels ()
import Data.IORef
import Data.List qualified as L
import Data.RadixTree.Word8.Strict qualified as T
import Data.Yaml as Y
import Debug.Trace (traceIO)
import Debug.TraceIf.Config.Type
import Language.Haskell.TH.Syntax
import System.Directory
import System.IO.Unsafe
import System.Environment (lookupEnv)

validateTraceMessageFormat :: TraceMessageFormatMaybe -> Maybe TraceMessageFormat
validateTraceMessageFormat ytc =
  TraceMessageFormat
  <$> ytc.entrySeparator
  <*> ytc.keyValueSeparator
  <*> ytc.retValPrefix
  <*> ytc.traceLinePattern

validateYamlConfig :: YamlConfigMaybe -> Maybe YamlConfig
validateYamlConfig yc =
  YamlConfig
  <$> yc.mode
  <*> yc.version
  <*> (yc.traceMessage >>= validateTraceMessageFormat)
  <*> yc.levels
  <*> yc.runtimeLevelsOverrideEnvVar

defaultTraceMessageFormatYaml :: TraceMessageFormatMaybe
defaultTraceMessageFormatYaml = TraceMessageFormat
  { entrySeparator = Just "; "
  , keyValueSeparator = Just ": "
  , retValPrefix = Just " => "
  , traceLinePattern =
    Just
    [ FullyQualifiedModule
    , Delimiter "::"
    , FunctionName
    , Delimiter ": "
    , LiteralMessage
    , Variables
    ]
  }

defaultTraceMessageFormat :: TraceMessageFormat
defaultTraceMessageFormat =
  maybe (error "defaultTraceMessageFormatYaml is partial") id $
    validateTraceMessageFormat defaultTraceMessageFormatYaml

newYamlConfig :: YamlConfigMaybe
newYamlConfig =
  YamlConfig
  { mode = Just TraceStd
  , version = Just 1
  , traceMessage = Just defaultTraceMessageFormatYaml
  , levels = Just [ LeveledModulePrefix Trace "" ]
  , runtimeLevelsOverrideEnvVar =
      Just (EnvironmentVariable "TRACE_IF_LEVEL_AMEND")
  }

loadYamlConfig :: IO YamlConfig
loadYamlConfig = do
  doesFileExist fp >>= \case
    True -> configFromJust . (<> nc) =<< catch (Y.decodeFileThrow fp) badYaml
    False -> do
      Y.encodeFile fp nc
      traceIO $ "New default config trace-if file is generated: [" <> fp <> "]"
      configFromJust nc
  where
    configFromJust :: YamlConfigMaybe -> IO YamlConfig
    configFromJust ycm =
      maybe (fail $ "YamlConfig is not valid: " <> show ycm) pure
      $ validateYamlConfig ycm
    badYaml e =
      fail $ "Fail to parse " <> show fp <> "file due:\n" <> prettyPrintParseException e
      <> "\nRename or delete existing config file to get default config."
    nc = newYamlConfig
    fp = traceIfConfigFileName

traceIfConfigFileName :: FilePath
traceIfConfigFileName = "trace-if.yaml"

traceIfConfigRef :: IORef (Maybe TraceIfConfig)
traceIfConfigRef = unsafePerformIO (newIORef Nothing)

-- TODO: Cache by file name instead of Maybe  to support multiple libraries
runtimeTraceIfConfigRef :: MVar (Maybe (T.StrictRadixTree TraceLevel))
runtimeTraceIfConfigRef = unsafePerformIO (newMVar Nothing)

mkPrefixTree :: [LeveledModulePrefix] -> T.StrictRadixTree TraceLevel
mkPrefixTree = L.foldl' go T.empty
  where
    go b e = T.insert (T.feedText e.modulePrefix) e.level b

yaml2Config :: YamlConfig -> TraceIfConfig
yaml2Config yc =
  TraceIfConfig (yc.mode) (yc.traceMessage)
  (mkPrefixTree $ yc.levels)
  (yc.runtimeLevelsOverrideEnvVar)

getConfig :: Q TraceIfConfig
getConfig = go
  where
    go = runIO (readIORef traceIfConfigRef) >>= \case
      Nothing -> do
        c <- yaml2Config <$> runIO loadYamlConfig
        runIO (atomicWriteIORef traceIfConfigRef (Just c))
        addDependentFile traceIfConfigFileName
        pure c
      Just c -> pure c


traceAll :: [LeveledModulePrefix]
traceAll = emptyPrefixTraceLevel Trace

emptyPrefixTraceLevel :: TraceLevel -> [LeveledModulePrefix]
emptyPrefixTraceLevel tl =
  [ LeveledModulePrefix
    { level = tl
    , modulePrefix = ""
    }
  ]

loadRuntimeConfig :: String -> IO (T.StrictRadixTree TraceLevel)
loadRuntimeConfig evar = do
  lookupEnv evar >>= \case
    Nothing -> pure $ mkPrefixTree traceAll
    Just "" -> pure T.empty
    Just "-" -> pure T.empty
    Just fp -> mkPrefixTree <$> loadRuntimeConfigFromYamlFile fp

loadRuntimeConfigFromYamlFile :: FilePath -> IO [LeveledModulePrefix]
loadRuntimeConfigFromYamlFile [] = pure [] -- disable all logs
loadRuntimeConfigFromYamlFile fp =
  doesFileExist fp >>= \case
    True ->
      catch (Y.decodeFileThrow fp) badYaml <* (traceIO $ "trace-if runtime config loaded from "
                                                <> show fp)
    False -> do
      traceIO $ "trace-if runtime config file " <> show fp
        <> " is missing - disable tracing"
      pure traceAll
  where
    badYaml e =
      fail $ "Fail to parse trace-if runtime config from file " <> show fp <> " due:\n"
      <> prettyPrintParseException e

getRuntimeConfig :: String -> IO (T.StrictRadixTree TraceLevel)
getRuntimeConfig evar = do
  readMVar runtimeTraceIfConfigRef >>= \case
    Just prTree -> pure prTree
    Nothing -> modifyMVar runtimeTraceIfConfigRef go >>= \case
      Nothing -> fail $ "getRuntimeConfig infinite recursion"
      Just r -> pure r
  where
    go = \case
      j@(Just _) -> pure (j, j)
      Nothing -> loadRuntimeConfig evar >>= \c -> pure (Just c, Just c)

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
module Debug.TraceIf.Config.Load where

import Control.Concurrent.MVar
import Control.Exception
import Data.Cache.LRU as LRU
import Data.Char
import Data.Generics.Labels ()
import Data.IORef
import Data.List qualified as L
import Data.RadixTree.Word8.Strict qualified as T
import Data.Yaml as Y
import Debug.Trace (traceIO)
import Debug.TraceIf.Config.Type
import Language.Haskell.TH
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
  , runtimeLevelsOverrideEnvVar = Just CapsPackageName
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

newtype DynConfigEnvVar = DynConfigEnvVar String deriving (Eq, Show, Ord, Lift)

runtimeTraceIfConfigRef :: MVar (LRU DynConfigEnvVar (T.StrictRadixTree TraceLevel))
runtimeTraceIfConfigRef = unsafePerformIO (newMVar $ newLRU (Just 7))

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

loadRuntimeConfig :: DynConfigEnvVar -> IO (T.StrictRadixTree TraceLevel)
loadRuntimeConfig (DynConfigEnvVar evar) = do
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

getRuntimeConfig :: DynConfigEnvVar -> IO (T.StrictRadixTree TraceLevel)
getRuntimeConfig evar = modifyMVar runtimeTraceIfConfigRef go
  where
    go lru =
      case LRU.lookup evar lru of
        (lru', Just dynCon) -> pure (lru', dynCon)
        (_, Nothing) -> loadRuntimeConfig evar >>=
          \c -> pure (LRU.insert evar c lru, c)

markerConfig :: TraceIfConfig
markerConfig = TraceIfConfig
    { mode = TraceEvent
    , traceMessage = TraceMessageFormat "" "" "" [ ModuleName, Delimiter "::", FunctionName ]
    , levels = mkPrefixTree traceAll
    , runtimeLevelsOverrideEnvVar = Ignored
    }

envVarName :: Loc -> EnvironmentVariable -> Maybe DynConfigEnvVar
envVarName loc = fmap DynConfigEnvVar . \case
  Ignored -> Nothing
  CapsPackageName ->
    Just . (packageBasedEnvVarPrefix <>)
    $ toUpper . underscoreNonAlphaNum
    <$> loc_package loc
  EnvironmentVariable evar -> Just evar
  where
    underscoreNonAlphaNum c
      | isAlphaNum c = c
      | otherwise = '_'

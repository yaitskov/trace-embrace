{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
module Debug.TraceEmbrace.Config.Load where

import Control.Concurrent
import Control.Exception
import Data.Cache.LRU as LRU
import Data.Char
import Data.Generics.Labels ()
import Data.IORef
import Data.List qualified as L
import Data.RadixTree.Word8.Strict qualified as T
import Data.Yaml as Y
import Debug.Trace (traceIO)
import Debug.TraceEmbrace.Config.Type
import Debug.TraceEmbrace.Config.Validation
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Refined
import System.Directory
import System.Environment (lookupEnv)
import System.IO
import System.IO.Unsafe


validateTraceMessageFormat ::
  String ->
  TraceMessageFormatMaybe ->
  Either String (Refined IdPred TraceMessageFormat)
validateTraceMessageFormat fieldName ytc =
  refineS fieldName =<< TraceMessageFormat
  <$> required "entrySeparator" ytc.entrySeparator
  <*> required "keyValueSeparator" ytc.keyValueSeparator
  <*> required "retValPrefix" ytc.retValPrefix
  <*> required "traceLinePattern" ytc.traceLinePattern

validateYamlConfig :: YamlConfigMaybe -> Either String YamlConfig
validateYamlConfig yc =
  YamlConfig
  <$> required "mode" yc.mode
  <*> required "version" yc.version
  <*> (required  "traceMessage" yc.traceMessage >>=
       validateTraceMessageFormat "traceMessage" . unrefine @IdPred)
  <*> required "levels" yc.levels
  <*> required "runtimeLevelsOverrideEnvVar" yc.runtimeLevelsOverrideEnvVar

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
  either (error "defaultTraceMessageFormatYaml is partial") unrefine $
    validateTraceMessageFormat "traceMessageFormat" defaultTraceMessageFormatYaml

newYamlConfig :: YamlConfigMaybe
newYamlConfig =
  YamlConfig
  { mode = Just TraceStd
  , version = Just 1
  , traceMessage = Just defaultTraceMessageFormatYaml
  , levels = Just [ LeveledModulePrefix Trace "" ]
  , runtimeLevelsOverrideEnvVar = Just CapsPackageName
  }

defaultYamlConfig :: YamlConfigMaybe
defaultYamlConfig = newYamlConfig { version = Nothing }

loadYamlConfig :: IO YamlConfig
loadYamlConfig = do
  doesFileExist fp >>= \case
    True ->
      configFromJust . (<> defaultYamlConfig) =<< catch (Y.decodeFileThrow fp) badYaml
    False -> do
      catch (do
                Y.encodeFile fp newYamlConfig
                putStrLn $ "New default config trace-embrace file is generated: [" <> fp <> "]")
            (\e -> putStrLn $ "Failed to create config file [" <> fp <> "] due: " <>
              prettyPrintParseException e)
      configFromJust newYamlConfig
  where
    configFromJust :: YamlConfigMaybe -> IO YamlConfig
    configFromJust ycm =
      either (\e -> fail $ show ycm <> "\nNot valid due: " <> e) pure
        $ validateYamlConfig ycm
    badYaml e =
      fail $ "Fail to parse " <> show fp <> "file due:\n" <> prettyPrintParseException e
      <> "\nRename or delete existing config file to get default config."
    fp = traceEmbraceConfigFileName

traceEmbraceConfigFileName :: FilePath
traceEmbraceConfigFileName = "trace-embrace.yaml"

traceEmbraceConfigRef :: IORef (Maybe TraceEmbraceConfig)
traceEmbraceConfigRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE traceEmbraceConfigRef #-}

unsafeIoSink :: IORef (Maybe Handle)
unsafeIoSink = unsafePerformIO (newIORef Nothing)
{-# NOINLINE unsafeIoSink #-}

newtype DynConfigEnvVar = DynConfigEnvVar String deriving (Eq, Show, Ord, Lift)

runtimeTraceEmbraceConfigRef :: MVar (LRU DynConfigEnvVar (T.StrictRadixTree TraceLevel))
runtimeTraceEmbraceConfigRef = unsafePerformIO (newMVar $ newLRU (Just 7))
{-# NOINLINE runtimeTraceEmbraceConfigRef #-}

mkPrefixTree :: [LeveledModulePrefix] -> T.StrictRadixTree TraceLevel
mkPrefixTree = L.foldl' go T.empty
  where
    go b e = T.insert (T.feedText e.modulePrefix) e.level b

yaml2Config :: YamlConfig -> TraceEmbraceConfig
yaml2Config yc =
  TraceEmbraceConfig (unrefine $ yc.mode) (unrefine $ yc.traceMessage)
  (mkPrefixTree . unrefine $ yc.levels)
  (unrefine $ yc.runtimeLevelsOverrideEnvVar)

configReadToken :: MVar ()
configReadToken = unsafePerformIO (newMVar ())
{-# NOINLINE configReadToken #-}

getConfig :: Q TraceEmbraceConfig
getConfig = do
  c <- runIO readConfigRef >>= loadIfNothing
  runIO (doesFileExist traceEmbraceConfigFileName) >>= \case
    True -> addDependentFile traceEmbraceConfigFileName
    False -> reportWarning $ "Config File is missing - skip dependency"
  pure c
  where
    readConfigRef = readIORef traceEmbraceConfigRef
    loadIfNothing = \case
      Nothing -> runIO $ do
        bracket (takeMVar configReadToken)
          (putMVar configReadToken)
          (\() ->
             readConfigRef >>= \case
               Just c ->
                 pure c
               Nothing -> do
                 c <- yaml2Config <$> loadYamlConfig
                 (atomicWriteIORef traceEmbraceConfigRef (Just c))
                 pure c)
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
      catch (Y.decodeFileThrow fp) badYaml <* (traceIO $ "trace-embrace runtime config loaded from "
                                                <> show fp)
    False -> do
      traceIO $ "trace-embrace runtime config file " <> show fp
        <> " is missing - disable tracing"
      pure traceAll
  where
    badYaml e =
      fail $ "Fail to parse trace-embrace runtime config from file " <> show fp <> " due:\n"
      <> prettyPrintParseException e

getRuntimeConfig :: DynConfigEnvVar -> IO (T.StrictRadixTree TraceLevel)
getRuntimeConfig evar = modifyMVar runtimeTraceEmbraceConfigRef go
  where
    go lru =
      case LRU.lookup evar lru of
        (lru', Just dynCon) -> pure (lru', dynCon)
        (_, Nothing) -> loadRuntimeConfig evar >>=
          \c -> pure (LRU.insert evar c lru, c)

markerConfig :: TraceEmbraceConfig
markerConfig = TraceEmbraceConfig
    { mode = TraceEvent
    , traceMessage =
        TraceMessageFormat
        ($$(refineTH "e") :: Refined SeparatorValidator String)
        ($$(refineTH "e") :: Refined SeparatorValidator String)
        ($$(refineTH "e") :: Refined SeparatorValidator String)
        ($$(refineTH [ ModuleName, Delimiter "::", FunctionName ]) :: Refined NonEmpty [TraceMessageElement])
    , levels = mkPrefixTree traceAll
    , runtimeLevelsOverrideEnvVar = Ignored
    }

envVarName :: Loc -> EnvironmentVariable -> Maybe DynConfigEnvVar
envVarName loc = fmap DynConfigEnvVar . \case
  Ignored -> Nothing
  CapsPackageName ->
    Just . (packageBasedEnvVarPrefix <>) . dropSuffix
    $ toUpper . underscoreNonAlphaNum
    <$> loc_package loc
  EnvironmentVariable evar -> Just evar
  where
    dropSuffix ('_':h:t)
       | isDigit h = []
       | otherwise = '_' : (dropSuffix $ h : t)
    dropSuffix (o:t) =  o : dropSuffix t
    dropSuffix [] = []

    underscoreNonAlphaNum c
      | isAlphaNum c = c
      | otherwise = '_'

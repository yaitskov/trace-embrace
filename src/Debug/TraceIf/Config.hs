{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Debug.TraceIf.Config where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception
import Control.Lens hiding (levels)
import Data.Aeson
import Data.Generics.Labels ()
import Data.IORef
import Data.List qualified as L
import Data.RadixTree.Word8.Strict qualified as T
import Data.Text
import Data.Yaml as Y
import Debug.Trace (traceIO)
import GHC.Generics
import Language.Haskell.TH.Syntax
import System.Directory
import System.IO.Unsafe
import System.Environment (lookupEnv)

data SinkMode
  = TraceDisabled
  | TraceStd
  -- | TraceEvent
  -- | TraceBoth
  deriving (Eq, Show, Ord, Generic)

instance ToJSON SinkMode where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON SinkMode

-- | Name of environment variable name.
data EnvironmentVariable
  = Ignored
  | EnvironmentVariable
    { varName :: String }
  deriving (Eq, Show, Ord, Generic)

instance ToJSON EnvironmentVariable where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON EnvironmentVariable

data TraceMessageElement
  = LiteralMessage -- ^ Constant tracing message
  | Variables -- ^ Variables e.g. @; x: 123; y: 321@
  | FullyQualifiedModule -- ^ Full Haskell module name (e.g. @Data.Map.Strict@)
  | ModuleName -- ^ Unqualified Haskell module name (e.g. @Strict@)
  | ShortenJavaModule -- ^ @D.M.Strict@
  | PackageName -- ^ Cabal package name
  | FunctionName -- ^ Function or method name containing tracing
  | LineNumber -- ^ Line number with tracing
  | Delimiter String -- | TraceMessageElement delimiter
  deriving (Eq, Show, Generic)

instance ToJSON TraceMessageElement where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON TraceMessageElement

type family Columnar f a where
  Columnar Identity a = a
  Columnar Maybe a = Maybe a

data TraceMessageFormatG a
  = TraceMessageFormat
    { entrySeparator :: Columnar a String -- ^ @"; "@ is default
    , keyValueSeparator :: Columnar a String -- ^ @": "@ is default
    , retValPrefix :: Columnar a String -- ^ @" => "@
    , traceLinePattern :: Columnar a [TraceMessageElement]
    }

type TraceMessageFormat = TraceMessageFormatG Identity
type TraceMessageFormatMaybe = TraceMessageFormatG Maybe

-- watch out for derivation order: https://gitlab.haskell.org/ghc/ghc/-/issues/25798
deriving instance Generic TraceMessageFormatMaybe
deriving instance Generic TraceMessageFormat
deriving instance Show TraceMessageFormat
deriving instance Eq TraceMessageFormat
instance ToJSON TraceMessageFormat where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON TraceMessageFormat
deriving instance Show TraceMessageFormatMaybe
deriving instance Eq TraceMessageFormatMaybe

instance ToJSON TraceMessageFormatMaybe where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON TraceMessageFormatMaybe
instance Semigroup TraceMessageFormatMaybe where
  a <> b =
    TraceMessageFormat
    (a ^. #entrySeparator <|> b ^. #entrySeparator)
    (a ^. #keyValueSeparator <|> b ^. #keyValueSeparator)
    (a ^. #retValPrefix <|> b ^. #retValPrefix)
    (a  ^. #traceLinePattern <|> b ^. #traceLinePattern)

data TraceLevel
  = Trace
  | Info
  | Warning
  | Error
  | TracingDisabled
  deriving (Eq, Show, Ord, Lift, Generic)

instance ToJSON TraceLevel where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON TraceLevel

data LeveledModulePrefix
  = LeveledModulePrefix
    { level :: TraceLevel
    , modulePrefix :: Text
    } deriving (Eq, Show, Generic)

instance ToJSON LeveledModulePrefix where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON LeveledModulePrefix

data YamlConfigG a
  = YamlConfig
    { mode :: Columnar a SinkMode
    , version :: Columnar a Int
    , traceMessage :: Columnar a (TraceMessageFormatG a)
    , levels :: Columnar a [ LeveledModulePrefix ]
    , runtimeLevelsOverrideEnvVar :: Columnar a EnvironmentVariable
    }

type YamlConfig = YamlConfigG Identity
type YamlConfigMaybe = YamlConfigG Maybe

deriving instance Generic YamlConfigMaybe
deriving instance Generic YamlConfig
deriving instance Show YamlConfig
deriving instance Eq YamlConfig
instance ToJSON YamlConfig where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON YamlConfig
deriving instance Show YamlConfigMaybe
deriving instance Eq YamlConfigMaybe
instance ToJSON YamlConfigMaybe where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON YamlConfigMaybe

instance Semigroup YamlConfigMaybe where
  a <> b =
    YamlConfig
    (a ^. #mode <|> b ^. #mode)
    (a ^. #version <|> b ^. #version)
    (a ^. #traceMessage <> b ^. #traceMessage)
    (a ^. #levels <|> b ^. #levels)
    (a ^. #runtimeLevelsOverrideEnvVar <|> b ^. #runtimeLevelsOverrideEnvVar)

validateTraceMessageFormat :: TraceMessageFormatMaybe -> Maybe TraceMessageFormat
validateTraceMessageFormat ytc =
  TraceMessageFormat
  <$> ytc ^. #entrySeparator
  <*> ytc ^. #keyValueSeparator
  <*> ytc ^. #retValPrefix
  <*> ytc ^. #traceLinePattern

validateYamlConfig :: YamlConfigMaybe -> Maybe YamlConfig
validateYamlConfig yc =
  YamlConfig
  <$> yc ^. #mode
  <*> yc ^. #version
  <*> (yc ^. #traceMessage >>= validateTraceMessageFormat)
  <*> yc ^. #levels
  <*> yc ^. #runtimeLevelsOverrideEnvVar

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

data TraceIfConfig
  = TraceIfConfig
    { mode :: SinkMode
    , traceMessage :: TraceMessageFormat
    , levels :: T.StrictRadixTree TraceLevel
    , runtimeLevelsOverrideEnvVar :: EnvironmentVariable
    } deriving (Eq, Show, Generic)

traceIfConfigRef :: IORef (Maybe TraceIfConfig)
traceIfConfigRef = unsafePerformIO (newIORef Nothing)

-- TODO: Cache by file name instead of Maybe  to support multiple libraries
runtimeTraceIfConfigRef :: MVar (Maybe (T.StrictRadixTree TraceLevel))
runtimeTraceIfConfigRef = unsafePerformIO (newMVar Nothing)

mkPrefixTree :: [LeveledModulePrefix] -> T.StrictRadixTree TraceLevel
mkPrefixTree = L.foldl' go T.empty
  where
    go b e = T.insert (T.feedText $ e ^. #modulePrefix) (e ^. #level) b

yaml2Config :: YamlConfig -> TraceIfConfig
yaml2Config yc =
  TraceIfConfig (yc ^. #mode) (yc ^. #traceMessage)
  (mkPrefixTree $ yc ^. #levels)
  (yc ^. #runtimeLevelsOverrideEnvVar)

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
traceAll =
  [ LeveledModulePrefix
    { level = Trace
    , modulePrefix = ""
    }
  ]

loadRuntimeConfig :: String -> IO (T.StrictRadixTree TraceLevel)
loadRuntimeConfig evar = do
  lookupEnv evar >>= \case
    Nothing -> pure $ mkPrefixTree traceAll
    Just "" -> pure T.empty
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

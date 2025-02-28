{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module Debug.TraceIf.Config.Type where

import Control.Applicative
import Control.Lens hiding (levels)
import Data.Aeson hiding (Error)
import Data.Generics.Labels ()
import Data.RadixTree.Word8.Strict qualified as T
import Data.Text
import GHC.Generics
import Language.Haskell.TH.Syntax

data SinkMode
  = TraceDisabled
  | TraceStd
  | TraceEvent
  deriving (Eq, Show, Ord, Generic)

instance ToJSON SinkMode where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON SinkMode

packageBasedEnvVarPrefix :: String
packageBasedEnvVarPrefix = "TRACE_EMBRACE_"

-- | Name of environment variable name.
data EnvironmentVariable
  = Ignored
  -- | Use upcased package name non alphanum chars are replaced with @_@,
  -- plus @TRACE_EMBRACE_@ prefix
  | CapsPackageName -- ^
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

-- watch out for derivation order: https://gitlab.haskell.org/ghc/ghc/-/issues/25798
entrySeparator :: Lens' (TraceMessageFormatG a) (Columnar a String)
entrySeparator = lens (.entrySeparator) $ \x a -> x { entrySeparator = a }
keyValueSeparator :: Lens' (TraceMessageFormatG a) (Columnar a String)
keyValueSeparator = lens (.keyValueSeparator) $ \x a -> x { keyValueSeparator = a }
retValPrefix :: Lens' (TraceMessageFormatG a) (Columnar a String)
retValPrefix = lens (.retValPrefix) $ \x a -> x { retValPrefix = a }
traceLinePattern :: Lens' (TraceMessageFormatG a) (Columnar a [TraceMessageElement])
traceLinePattern = lens (.traceLinePattern) $ \x a -> x { traceLinePattern = a }

type TraceMessageFormat = TraceMessageFormatG Identity
type TraceMessageFormatMaybe = TraceMessageFormatG Maybe


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
    (a.entrySeparator <|> b.entrySeparator)
    (a.keyValueSeparator <|> b.keyValueSeparator)
    (a.retValPrefix <|> b.retValPrefix)
    (a.traceLinePattern <|> b.traceLinePattern)

data TraceLevel
  = Trace
  | Info
  | Warning
  | Error
  | TracingDisabled
  deriving (Eq, Show, Ord, Lift, Generic, Bounded, Enum)

traceLevelToChar :: TraceLevel -> Text
traceLevelToChar = \case
  Trace -> "-"
  Info -> ""
  Warning -> "!"
  Error -> "|"
  TracingDisabled -> "#"

charToLevel :: String -> (TraceLevel, String)
charToLevel [] = (TracingDisabled, "")
charToLevel s@(l:m)=
  case l of
    '-' -> (Trace, m)
    '!' -> (Warning, m)
    '|' -> (Error, m)
    '#' -> (TracingDisabled, m)
    _   -> (Info,  s)

data LeveledModulePrefix
  = LeveledModulePrefix
    { level :: TraceLevel
    , modulePrefix :: Text
    } deriving (Eq, Show, Generic)

instance ToJSON LeveledModulePrefix where
  toJSON o = String $ traceLevelToChar o.level <> o.modulePrefix
instance FromJSON LeveledModulePrefix where
  parseJSON (String x) =
    pure . uncurry LeveledModulePrefix . fmap pack . charToLevel $ unpack x
  parseJSON o =
    fail $ "Failed to parse [" <> show o
      <> "] as LeveledModulePrefix because String is expected"

data YamlConfigG a
  = YamlConfig
    { mode :: Columnar a SinkMode
    , version :: Columnar a Int
    , traceMessage :: Columnar a (TraceMessageFormatG a)
    , levels :: Columnar a [ LeveledModulePrefix ]
    , runtimeLevelsOverrideEnvVar :: Columnar a EnvironmentVariable
    }

-- mode :: Lens' (YamlConfigG a) (Columnar a SinkMode)
-- mode = lens (._mode) $ \x a -> x { _mode = a }
-- version :: Lens' (YamlConfigG a) (Columnar a Int)
-- version = lens _version $ \x a -> x { _version = a }
-- traceMessage :: Lens' (YamlConfigG a) (Columnar a  (TraceMessageFormatG a))
-- traceMessage = lens _traceMessage $ \x a -> x { _traceMessage = a }
-- levels :: Lens' (YamlConfigG a) (Columnar a [ LeveledModulePrefix ])
-- levels = lens _levels $ \x a -> x { _levels = a }
-- runtimeLevelsOverrideEnvVar :: Lens' (YamlConfigG a) (Columnar a EnvironmentVariable)
-- runtimeLevelsOverrideEnvVar = lens _runtimeLevelsOverrideEnvVar $ \x a -> x { _runtimeLevelsOverrideEnvVar = a }

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
    (a.mode <|> b.mode)
    (a.version <|> b.version)
    (a.traceMessage <> b.traceMessage)
    (a.levels <|> b.levels)
    (a.runtimeLevelsOverrideEnvVar <|> b.runtimeLevelsOverrideEnvVar)

data TraceIfConfig
  = TraceIfConfig
    { mode :: SinkMode
    , traceMessage :: TraceMessageFormat
    , levels :: T.StrictRadixTree TraceLevel
    , runtimeLevelsOverrideEnvVar :: EnvironmentVariable
    } deriving (Eq, Show, Generic)

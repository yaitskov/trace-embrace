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
import Refined

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
  deriving (Eq, Show, Generic, Lift)

instance ToJSON TraceMessageElement where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON TraceMessageElement

type family Columnar f r a where
  Columnar Identity r a = Refined r a
  Columnar Maybe _ a = Maybe a

type SeparatorValidator = And (SizeLessThan 5) NonEmpty
data TraceMessageFormatG a
  = TraceMessageFormat
    { entrySeparator :: Columnar a SeparatorValidator String -- ^ @"; "@ is default
    , keyValueSeparator :: Columnar a SeparatorValidator String -- ^ @": "@ is default
    , retValPrefix :: Columnar a SeparatorValidator String -- ^ @" => "@
    , traceLinePattern :: Columnar a NonEmpty [TraceMessageElement]
    }

-- watch out for derivation order: https://gitlab.haskell.org/ghc/ghc/-/issues/25798
entrySeparator :: Lens' (TraceMessageFormatG a) (Columnar a SeparatorValidator String)
entrySeparator = lens (.entrySeparator) $ \x a -> x { entrySeparator = a }
keyValueSeparator :: Lens' (TraceMessageFormatG a) (Columnar a SeparatorValidator String)
keyValueSeparator = lens (.keyValueSeparator) $ \x a -> x { keyValueSeparator = a }
retValPrefix :: Lens' (TraceMessageFormatG a) (Columnar a SeparatorValidator String)
retValPrefix = lens (.retValPrefix) $ \x a -> x { retValPrefix = a }
traceLinePattern :: Lens' (TraceMessageFormatG a) (Columnar a NonEmpty [TraceMessageElement])
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
charToLevel [] = (Info, "")
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
    { mode :: Columnar a IdPred SinkMode
    , version :: Columnar a (And (GreaterThan 0) (LessThan 2)) Int
    , traceMessage :: Columnar a IdPred (TraceMessageFormatG a)
    , levels :: Columnar a IdPred [ LeveledModulePrefix ]
    , runtimeLevelsOverrideEnvVar :: Columnar a IdPred EnvironmentVariable
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

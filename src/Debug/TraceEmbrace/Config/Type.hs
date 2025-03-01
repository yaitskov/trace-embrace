{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module Debug.TraceEmbrace.Config.Type
  ( module E
  , YamlConfigG (..)
  , YamlConfig
  , YamlConfigMaybe
  , TraceEmbraceConfig (..)
  ) where

import Control.Applicative
import Control.Lens hiding (levels)
import Data.Aeson hiding (Error)
import Data.Generics.Labels ()
import Data.RadixTree.Word8.Strict qualified as T
import Debug.TraceEmbrace.Config.Type.Mode as E
import Debug.TraceEmbrace.Config.Type.EnvVar as E
import Debug.TraceEmbrace.Config.Type.Level as E
import Debug.TraceEmbrace.Config.Type.TraceMessage as E
import Debug.TraceEmbrace.Config.Validation
import GHC.Generics
import Refined

data YamlConfigG a
  = YamlConfig
    { mode :: Columnar a SinkModeP SinkMode
    , version :: Columnar a (And (GreaterThan 0) (LessThan 2)) Int
    , traceMessage :: Columnar a IdPred (TraceMessageFormatG a)
    , levels :: Columnar a HaskellModulePrefixP [ LeveledModulePrefix ]
    , runtimeLevelsOverrideEnvVar :: Columnar a EnvironmentVariableP EnvironmentVariable
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

data TraceEmbraceConfig
  = TraceEmbraceConfig
    { mode :: SinkMode
    , traceMessage :: TraceMessageFormat
    , levels :: T.StrictRadixTree TraceLevel
    , runtimeLevelsOverrideEnvVar :: EnvironmentVariable
    } deriving (Eq, Show, Generic)

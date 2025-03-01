{-# LANGUAGE OverloadedStrings #-}
module Debug.TraceEmbrace.Config.Type.EnvVar where

import Data.Aeson hiding (Error)
import Data.Char
import Data.Generics.Labels ()
import Data.Text qualified as T
import Data.Typeable
import GHC.Generics

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
  | EnvironmentVariable { varName :: String }
  deriving (Eq, Show, Ord, Generic)

instance ToJSON EnvironmentVariable where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON EnvironmentVariable


data EnvironmentVariableP

instance Predicate EnvironmentVariableP EnvironmentVariable where
  validate p = \case
    Ignored -> Nothing
    CapsPackageName -> Nothing
    EnvironmentVariable n ->
      case n of
        [] -> throwRefineOtherException (typeRep p) "Environment variable name cannot be empty"
        (h:t)
          | isUpper h && all (\c -> isUpper c || c == '_' || isDigit c) t -> Nothing
          | otherwise ->
            throwRefineOtherException (typeRep p)
              ("Bad environment variable name: " <> T.pack (show n))

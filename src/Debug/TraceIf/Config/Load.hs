{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
module Debug.TraceIf.Config.Load where


import Control.Concurrent.MVar
import Control.Exception
import Control.Lens
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
import Refined
import System.Directory
import System.Environment (lookupEnv)
import System.IO.Unsafe

type family Col f r a where
  Col Identity r a = Refined r a
  Col Maybe r a = Maybe a

data Foo a  = Foo
    { foo1 :: Col a (SizeLessThan 4) String -- ^ @"; "@ is default
    , foo2 :: Col a (NonEmpty) String -- ^ @": "@ is default
    -- , retValPrefix :: Columnar a String -- ^ @" => "@
    -- , traceLinePattern :: Columnar a [TraceMessageElement]
    }

type FooMay = Foo Maybe
type FooI = Foo Identity

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = \case
  Left x -> Left $ f x
  Right o -> Right o

refineS :: forall {k} (p :: k) x. Predicate p x => String -> x -> Either String (Refined p x)
refineS fieldName =
  mapLeft ((("Field [" <> fieldName <> "] is not valid due: ") <>) . show) . refine

required :: forall {k} {p :: k} {a}. Predicate p a => String -> Maybe a -> Either String (Refined p a)
required s v =
  (maybe (Left $ "[" <> s <> "] field is required") pure v) >>= refineS s

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
    True -> configFromJust . (<> defaultYamlConfig) =<< catch (Y.decodeFileThrow fp) badYaml
    False -> do
      Y.encodeFile fp newYamlConfig
      traceIO $ "New default config trace-if file is generated: [" <> fp <> "]"
      configFromJust newYamlConfig
  where
    configFromJust :: YamlConfigMaybe -> IO YamlConfig
    configFromJust ycm =
      either (\e -> fail $ show ycm <> "\nNot valid due: " <> e) pure
        $ validateYamlConfig ycm
    badYaml e =
      fail $ "Fail to parse " <> show fp <> "file due:\n" <> prettyPrintParseException e
      <> "\nRename or delete existing config file to get default config."
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
  TraceIfConfig (unrefine $ yc.mode) (unrefine $ yc.traceMessage)
  (mkPrefixTree . unrefine $ yc.levels)
  (unrefine $ yc.runtimeLevelsOverrideEnvVar)

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
    Just . (packageBasedEnvVarPrefix <>)
    $ toUpper . underscoreNonAlphaNum
    <$> loc_package loc
  EnvironmentVariable evar -> Just evar
  where
    underscoreNonAlphaNum c
      | isAlphaNum c = c
      | otherwise = '_'

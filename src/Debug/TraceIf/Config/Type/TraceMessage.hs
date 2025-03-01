{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Debug.TraceIf.Config.Type.TraceMessage where

import Control.Applicative
import Control.Lens hiding (levels)
import Data.Aeson hiding (Error)
import Data.Generics.Labels ()
import Debug.TraceIf.Config.Validation
import GHC.Generics
import Language.Haskell.TH.Syntax
import Refined

data TraceMessageElement
  = LiteralMessage -- ^ Constant tracing message
  | Variables -- ^ Variables e.g. @; x: 123; y: 321@
  | FullyQualifiedModule -- ^ Full Haskell module name (e.g. @Data.Map.Strict@)
  | ModuleName -- ^ Unqualified Haskell module name (e.g. @Strict@)
  | ShortenJavaModule -- ^ @D.M.Strict@
  | PackageName -- ^ Cabal package name
  | FunctionName -- ^ Function or method name containing tracing
  | LineNumber -- ^ Line number with tracing
  | TimeStamp { format :: String } -- ^ @%H:%M:%S.%q@
  | Delimiter String -- | TraceMessageElement delimiter
  deriving (Eq, Show, Generic, Lift)

instance ToJSON TraceMessageElement where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON TraceMessageElement

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

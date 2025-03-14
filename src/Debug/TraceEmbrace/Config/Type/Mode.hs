{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}
module Debug.TraceEmbrace.Config.Type.Mode where

import Data.Aeson hiding (Error)
import Data.Generics.Labels ()
import Data.Typeable
import GHC.Generics
import Language.Haskell.TH.Syntax
import Refined

data IoSink
  = StdErrSink
  | StdOutSink
  | FileSink FilePath deriving (Eq, Show, Generic, Lift)

instance ToJSON IoSink where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON IoSink

data SinkMode
  = TraceDisabled
  | TraceStd
  | TraceUnsafeIo { sink :: IoSink }
  | TraceEvent
  deriving (Eq, Show, Generic, Lift)

instance ToJSON SinkMode where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON SinkMode

data SinkModeP

instance Predicate SinkModeP SinkMode where
  validate p = \case
    TraceDisabled -> Nothing
    TraceStd -> Nothing
    TraceEvent -> Nothing
    TraceUnsafeIo s -> case s of
      StdErrSink -> Nothing
      StdOutSink -> Nothing
      FileSink "" -> throwRefineOtherException (typeRep p) "Sink path is empty"
      FileSink _ -> Nothing

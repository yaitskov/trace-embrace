{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
module Debug.TraceEmbrace.Config.Type.Level where

import Control.Monad
import Data.Aeson hiding (Error)
import Data.Char
import Data.Generics.Labels ()
import Data.List qualified as L
import Data.Maybe
import Data.Text qualified as T
import Data.Typeable
import GHC.Generics
import Language.Haskell.TH.Syntax
import Refined


data TraceLevel
  = Trace
  | Info
  | Warning
  | Error
  | TracingDisabled
  deriving (Eq, Show, Ord, Lift, Generic, Bounded, Enum)

traceLevelToChar :: TraceLevel -> T.Text
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

data HaskellModulePrefixP

data LeveledModulePrefix
  = LeveledModulePrefix
    { level :: TraceLevel
    , modulePrefix :: T.Text
    } deriving (Eq, Show, Generic)

instance Predicate HaskellModulePrefixP LeveledModulePrefix where
  validate pr p = case T.uncons p.modulePrefix of
    Nothing -> Nothing
    Just _
      | T.any (\c -> not (isAlphaNum c) && c /= '_' && c /= '.') p.modulePrefix ->
          throwRefineOtherException (typeRep pr)
            ("Module prefix can contain letters, digits, dots and underbars only but: ["
              <> p.modulePrefix <> "]")
      | any (not . isUpper . fst . fromMaybe ('A', "") . T.uncons) $ T.split (== '.') p.modulePrefix ->
          throwRefineOtherException (typeRep pr)
                 ("Module prefix segment should start with a capital letter but: ["
                  <> p.modulePrefix <> "]")
      | otherwise -> Nothing

instance Predicate HaskellModulePrefixP [LeveledModulePrefix] where
  validate pr pp = join $ L.find isJust (validate pr<$> pp)

instance ToJSON LeveledModulePrefix where
  toJSON o = String $ traceLevelToChar o.level <> o.modulePrefix
instance FromJSON LeveledModulePrefix where
  parseJSON (String x) =
    pure . uncurry LeveledModulePrefix . fmap T.pack . charToLevel $ T.unpack x
  parseJSON o =
    fail $ "Failed to parse [" <> show o
      <> "] as LeveledModulePrefix because String is expected"

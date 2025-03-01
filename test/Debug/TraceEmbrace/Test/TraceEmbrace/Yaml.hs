{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Debug.TraceEmbrace.Test.TraceEmbrace.Yaml where

import Data.Text
import Data.Yaml as Y
import Debug.TraceEmbrace.Config.Type.Level
import Test.Tasty.HUnit ((@=?))
import Test.Tasty.QuickCheck

instance Arbitrary TraceLevel where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Text where
  arbitrary = pack . getUnicodeString <$> arbitrary

prop_dyn_conf_marshalling :: TraceLevel -> Text -> Bool
prop_dyn_conf_marshalling l t =
  case Y.decodeEither' (Y.encode e) of
    Left err -> error $ show err
    Right e' -> e' == e
  where
    e = LeveledModulePrefix l t

unit_dyn_conf_golden :: IO ()
unit_dyn_conf_golden = Y.encode l @=? "- '|Data'\n- '!Control'\n- System.\n- -Unsafe\n"
  where
    l = [ LeveledModulePrefix Error "Data"
        , LeveledModulePrefix Warning "Control"
        , LeveledModulePrefix Info "System."
        , LeveledModulePrefix Trace "Unsafe"
        ]

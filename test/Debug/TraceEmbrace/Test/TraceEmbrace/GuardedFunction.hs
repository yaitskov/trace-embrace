-- {-# OPTIONS_GHC -ddump-splices #-}
module Debug.TraceEmbrace.Test.TraceEmbrace.GuardedFunction where

import Debug.TraceEmbrace
import Debug.TraceEmbrace.Test.TraceEmbrace.Config
import Language.Haskell.TH.Lock
import Test.Tasty.HUnit ((@=?))

ensureSerialCompilationVerbose

sum4 :: (Show a, Num a) => a -> a -> a -> a -> a
sum4 $a $a $s_ $a | $tg = $u
sum4 a1 a2 a3 a4 = a1 + a2 + a3 + a4

unit_sum4_a_tg_u :: IO ()
unit_sum4_a_tg_u = withPrefixEnvVar thresholdConfig "" $ sum4 one one one one @=? 4

sum1 :: Show a => a -> a
sum1 $a | $tg = $u
sum1 a1 = a1

unit_sum1_a_tg_u :: IO ()
unit_sum1_a_tg_u = withPrefixEnvVar thresholdConfig "" $ sum1 one @=? 1

sum2 :: (Show a, Num a) => a -> a -> a
sum2 $a $a | $(tg' "sum2/") = $u
sum2 a1 a2 = a1 + a2

unit_sum2_a_tg_u :: IO ()
unit_sum2_a_tg_u = withPrefixEnvVar thresholdConfig "" $ sum2 one one @=? 2

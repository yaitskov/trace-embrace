{-# OPTIONS_HADDOCK hide #-}
-- | Alternative workaround to avoid unused warnings for
-- types and values referred only in HADDOCKS
module Debug.TraceEmbrace.Haddock (HaddockRefs (..), module E) where

import Data.Proxy as E
import Data.Tagged as E

class HaddockRefs a where
  usedSomeHow :: Proxy a -> Int

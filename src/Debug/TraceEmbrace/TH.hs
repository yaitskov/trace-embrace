{-# OPTIONS_HADDOCK hide, prune #-}

-- | Tracing with TH
module Debug.TraceEmbrace.TH (tr, tw, tw', trIo, trFunMarker, trIoFunMarker) where

import Debug.Trace
import Debug.TraceEmbrace.Config
import Debug.TraceEmbrace.Haddock
import Debug.TraceEmbrace.Internal.Rewrap
import Debug.TraceEmbrace.Internal.TH qualified as I
import Language.Haskell.TH

data Ref

instance HaddockRefs Ref where
  usedSomeHow _ = length ['TraceMessageFormat, ''Rewrap, 'trace, 'traceEvent]

-- | TH version of 'trace' and 'traceEvent'
-- The message is formatted according to 'TraceMessageFormat'.
-- The generated expression has type @forall r (a :: TYPE r) b a. 'Rewrap' a b => a -> a@.
-- 'id' is generated if effective trace level is lower than trace level threshold.
-- Example:
--
-- > foo x = $(tr "get/x") x
--
-- Output:
--
-- > Module::foo get; x : 132
tr :: String -> Q Exp
tr = I.tr [| \x -> x |]


-- | TH version of 'traceWith' and 'traceEventWith'
-- The message is formatted according to 'TraceMessageFormat'.
-- The generated expression has type @forall r (a :: TYPE r) b a. (Show a, Rewrap a b) => a -> a@.
-- 'id' is generated if effective trace level is lower than trace level threshold.
-- Example:
--
-- > foo x = $(tw "get/x") (x + 1)
--
-- Output:
--
-- > Module::foo get; x : 132 => 133
tw :: String -> Q Exp
tw = I.tw [| \x -> x |]

-- | Like 'tw' but return value is wrapped with 'ShowTrace'.
tw' :: String -> Q Exp
tw' = I.tw' [| \x -> x |]

-- | TH version of 'traceIO' and 'traceEventIO'
-- The message is formatted according to 'TraceMessageFormat'.
-- Example:
--
-- > foo x = $(trIo "get/x") >> pure x
--
-- Output:
--
-- > Module::foo get; x : 132
trIo :: String -> Q Exp
trIo = I.trIo [| pure () |]

-- | TH version of 'traceMarker' where module and function
-- are used as a marker. Trace level is used.
trFunMarker :: Q Exp
trFunMarker = I.trFunMarker [| \x -> x |]

-- | TH version of 'traceMarkerIO' where module and function
-- are used as a marker. Trace level is not used.
trIoFunMarker :: Q Exp
trIoFunMarker = I.trIoFunMarker [| pure () |]

-- | Tracing with TH
module Debug.TraceIf.TH (tr, tw, trIo) where

import Debug.TraceIf.Internal.TH qualified as I
import Language.Haskell.TH

-- | TH version of 'trace' and 'traceEvent'
-- The message is formatted according to 'TraceMessageFormat'.
-- The generated expression has type @forall r (a :: TYPE r) b a. Rewrap a b => a -> a@.
-- 'id' is generated if effective trace level is lower than trace level threshold.
-- Example:
--
-- > foo x = $(tr "get/x") x
--
-- Output:
--
-- > Main::foo get; x : 132
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
-- > Main::foo get; x : 132 => 133
tw :: String -> Q Exp
tw = I.tw [| \x -> x |]

-- | TH version of 'traceIO' and 'traceEventIO'
-- The message is formatted according to 'TraceMessageFormat'.
-- Example:
--
-- > foo = $(trIo "get/x") >> pure 1
--
-- Output:
--
-- > Main::foo get; x : 132
trIo :: String -> Q Exp
trIo = I.trIo [| pure () |]

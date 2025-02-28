-- | Tracing with TH
module Debug.TraceIf.TH (tr, tw) where

import Debug.TraceIf.Internal.TH qualified as I
import Language.Haskell.TH

-- | TH version of 'trace'
-- The message is formatted according to 'TraceMessageFormat'.
-- The generated expression has type @forall r (a :: TYPE r) b a. Rewrap a b => a -> a@.
-- 'id' is generated if effective trace level is lower than trace level threshold.
-- Example:
--
-- > foo x = $(tr "get/x") x
--
tr :: String -> Q Exp
tr = I.tr [| \x -> x |]


-- | TH version of 'traceWith'
-- The message is formatted according to 'TraceMessageFormat'.
-- The generated expression has type @forall r (a :: TYPE r) b a. (Show a, Rewrap a b) => a -> a@.
-- 'id' is generated if \"NOTRACE\" environment variable is defined.
-- tw :: String -> Q Exp
tw :: String -> Q Exp
tw = I.tw [| \x -> x |]

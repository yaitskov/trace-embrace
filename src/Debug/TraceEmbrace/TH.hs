{-# OPTIONS_HADDOCK hide, prune #-}

-- | Tracing with TH
module Debug.TraceEmbrace.TH (tr, tw, tw', trIo, u, a, s_, tg, tg', underbar, trFunMarker, trIoFunMarker) where

import Debug.Trace
import Debug.TraceEmbrace.Config
import Debug.TraceEmbrace.FileIndex (FunName (..))
import Debug.TraceEmbrace.Internal.Rewrap
import Debug.TraceEmbrace.Internal.TH qualified as I
import Haddock.UseRefs
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

countDocRefs

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
tr tm = I.tr [| \x -> x |] tm

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

data ArgPatCounter
  = ArgPatCounter
    { funName :: FunName
    , argNames :: [Name]
    } deriving (Show)

data Undebar = Undebar

instance Show Undebar where
  show _ = "_"

underbar :: Undebar
underbar = Undebar
-- | Generates consequent pattern variable for tracing arguments of a guarded function.
-- It is assumed that 'a' is used together with 'tg' and 'u'.
--
-- > foo $a $a $a | $tg = $u
-- > foo 0  _  _ = 0
-- > foo x y z = x + y + z
--
a :: Q Pat
a = do
  cfn <- I.currentFunName
  getQ >>= \case
    Nothing -> reset cfn
    Just (ArgPatCounter fn aNames)
      | fn == cfn -> go aNames cfn
      | otherwise -> reset cfn
  where
    reset cfn = go [] cfn
    go ans cfn = do
      nn <- newName $ "traceEmbracePatArg" <> (show $ length ans)
      putQ (ArgPatCounter cfn $ nn : ans)
      varP nn

-- | Similar to 'a', but argument is not included in trace message.
s_ :: Q Pat
s_ = do
  cfn <- I.currentFunName
  getQ >>= \case
    Nothing -> reset cfn
    Just (ArgPatCounter fn aNames)
      | fn == cfn -> go aNames cfn
      | otherwise -> reset cfn
  where
    reset cfn = go [] cfn
    go ans cfn = putQ (ArgPatCounter cfn $ 'underbar : ans) >> [p|_|]

-- | Expands to @$(tr "/a b c d...") False@
tg :: Q Exp
tg = tg' ""

-- | Similar to 'tg' with message prefix with the argument.
tg' :: String -> Q Exp
tg' msgPrefix = do
  cfn <- I.currentFunName
  getQ >>= \case
    Nothing -> er
    Just (ArgPatCounter fn aNames)
      | fn == cfn -> [|$(I.tr' [| \x -> x |] msgPrefix $ reverse aNames) False|]
      | otherwise -> er
  where
    er = fail "Use 'Debug.TraceEmbrace.TH.a' macro to capture function arguments before calling 'tg'"

-- | Shortcut for 'undefined'
u :: Q Exp
u = [| undefined |]

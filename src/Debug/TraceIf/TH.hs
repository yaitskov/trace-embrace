{-# LANGUAGE TemplateHaskell #-}
-- | Tracing with TH
module Debug.TraceIf.TH (svars, tr, tw) where

import Debug.TraceIf.If
import Debug.TraceIf.Show
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Printf

showTrace :: Show (ShowTrace a) => a -> String
showTrace = show . ShowTrace

{- | Interpolate vars in the arugment and prepend with TH splice location.
Generated expression has type 'String'.
The argument has literal and interpolated parts.
There parts are separated with right slash (/).

@
foo x y = trace $(svars "foo get/x y") x
@

The snippet above is expanded into:

@
foo x y = trace (" 99:Main foo get; x: " <> show x <> "; y: " <> show y) x
@

'Show' instance of some types (eg lazy ByteString) hide
internal structure which might be important in low level code.
Variables after "#" are wrapped into t'ShowTrace':

@
import Data.ByteString.Lazy
foo x = trace $(svars "foo get/x#x") x
@

The snippet above is expanded into:

@
foo x = trace (" 99:Main foo get; x: " <> show x <> "; x: " <> show (ShowTrace y)) x
@

-}
svars :: String -> Q Exp
svars s = do
  l :: String <- locToStr literalPart <$> location
  case interpolatePart of
    '/':vars ->
      case span ('#' /=) vars of
        (showVars, '#' : traceVars) ->
          [|mconcat (l : $(listE (wordsToVars 'show (words showVars)
                                  <> wordsToVars 'showTrace (words traceVars)))) :: String|]
        (showVars, "") ->
          [|mconcat (l : $(listE (wordsToVars 'show (words showVars)))) :: String|]
        (sv, st) -> fail $ printf "No case for %s %s" sv st
    _ ->
      fail $ printf "Interpolation part is empty in: [%s]" s
  where
    locToStr lpart l = printf "%3d:%s %s" (fst $ loc_start l) (loc_module l) lpart
    (literalPart, interpolatePart) = span ('/' /=) s
    wordsToVars f = fmap go
      where
        go vs =
          lookupValueName vs >>= \case
            Nothing -> do
              reportError $ printf "no variable [%s]" vs
              [| $(lift vs) |]
            Just vn ->
              [|"; " <> $(lift vs) <> ": " <> $(varE f) $(varE vn)|]

-- | TH version of 'trace'
-- The argument is processed with 'svars'.
-- Generated expression has type @a -> a@.
-- 'id' is generated if \"NOTRACE\" environment variable is not defined.
-- Example:
--
-- > foo x = $(tr "foo get/x") x
--
-- Expanded into:
--
-- > foo x = trace $(svars "foo get/x") x
--
tr :: String -> Q Exp
tr s
  | isTracingEnabled = [|trace $(svars s)|]
  | otherwise = [|id|]

-- | TH version of 'traceWith'
-- The argument is processed with 'svars'.
-- Generated expression has type @Show a => a -> a@.
-- 'id' is generated if \"NOTRACE\" environment variable is defined.

tw :: String -> Q Exp
tw s
  | isTracingEnabled = [|traceWith ((($(svars s) <> " => ") <>) . show) |]
  | otherwise = [|id|]

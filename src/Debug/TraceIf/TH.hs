{-# LANGUAGE TemplateHaskell #-}

-- | Tracing with TH
module Debug.TraceIf.TH (svars, tr, tw, svarsWith, traceMessage) where

import Data.Char as C
import Debug.Trace qualified as T
import Debug.TraceIf.If
import Debug.TraceIf.Show
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Prelude hiding (Show (..))
import Text.Printf

showTrace :: Show (ShowTrace a) => a -> String
showTrace = show . ShowTrace

-- | Extract var names from a word.
--
-- @
-- "_" => []
-- "0" => []
-- "(Just" => []
-- "x@[a,_c]"=> ["x", "a"]
-- "l@(h:t)"  => ["l", "h", "t"]
-- "{a,b}"    => ["a", "b"]
-- @
--
varNamesFromPat :: String -> [String]
varNamesFromPat = filterVars . words . fmap replaceWithSpace . stripStrComment
  where
    filterVars = filter (\case { h:_ -> C.isLower h; [] -> False; })
    replaceWithSpace c
      | c `elem` ",!@({[:]})~" = ' '
      | otherwise = c

    dropTillEndOfString = \case
      "" -> ""
      '\\' : '"' : t -> dropTillEndOfString t
      '"' : t -> t
      _ : t -> dropTillEndOfString t

    dropTillEndOfLine = \case
      "" -> ""
      '\n' : t -> t
      _ : t -> dropTillEndOfLine t

    dropTillEndOfComment = \case
      "" -> ""
      '-' : '}' : t -> t
      '{' : '-' : t -> dropTillEndOfComment $ dropTillEndOfComment t
      _ : t -> dropTillEndOfComment t

    stripStrComment = \case
      "" -> ""
      '"' : t -> stripStrComment $ dropTillEndOfString t
      '-' : '-' : t -> stripStrComment $ dropTillEndOfLine t
      '{' : '-' : t -> stripStrComment $ dropTillEndOfComment t
      h : t -> h : stripStrComment t


{- | Interpolate vars in the arugment.
Generated expression has type 'String'.
The argument has literal and interpolated parts.
There parts are separated with right slash (/).

@
foo x y = trace $(svars "foo get/x y") x
@

The snippet above is expanded into:

@
foo x y = trace ("foo get; x: " <> show x <> "; y: " <> show y) x
@

'Show' instance of some types (eg lazy ByteString) hide
internal structure which might be important in low level code.
Variables after ";" are wrapped into t'ShowTrace':

@
import Data.ByteString.Lazy
foo x = trace $(svars "foo get/x;x") x
@

The snippet above is expanded into:

@
foo x = trace ("foo get; x: " <> show x <> "; x: " <> show (ShowTrace y)) x
@

-}
svars :: String -> Q Exp
svars s = do
  case interpolatePart of
    '/':vars ->
      case span (';' /=) vars of
        (showVars, ';' : traceVars) ->
          [|(mconcat
            ($(lift literalPart) :
              $(listE (wordsToVars 'show showVars
                        <> wordsToVars 'showTrace traceVars)))) :: String|]
        (showVars, "") ->
          [|(mconcat
            ($(lift literalPart)
              : $(listE (wordsToVars 'show showVars)))) :: String|]
        (sv, st) -> fail $ printf "No case for %s %s" sv st
    _ ->
      fail $ printf "Interpolation part is empty in: [%s]" s
  where
    (literalPart, interpolatePart) = span ('/' /=) s
    wordsToVars f vss = fmap go (varNamesFromPat vss)
      where
        go vs =
          lookupValueName vs >>= \case
            Nothing -> do
              reportError $ printf "no variable [%s]" vs
              [| $(lift vs) |]
            Just vn ->
              [|"; " <> $(lift vs) <> ": " <> $(varE f) $(varE vn)|]

-- | Suffix 'svars' with return value.
svarsWith :: String -> Q Exp
svarsWith s = [| \x -> ((($(svars s) <> " => ") <>) (show x)) {-:: (Rewrap a b, Show a) => a -> String-} |]

-- | Splice location.
traceMessage :: Q Exp
traceMessage = lift =<< locToStr <$> location
  where
    locToStr :: Loc -> String
    locToStr l = printf "%3d:%s " (fst $ loc_start l) (loc_module l)

-- | TH version of 'trace'
-- The argument is processed with 'svars'.
-- Generated expression has type @forall r (a :: TYPE r) b a. Rewrap a b => a -> a@.
-- 'id' is generated if \"NOTRACE\" environment variable is not defined.
-- Example:
--
-- > foo x = $(tr "foo get/x") x
--
tr :: String -> Q Exp
tr s
  | isTracingEnabled =
    [| \x ->
        if isTracingEnabled
        then unwrap (T.trace ($(traceMessage) <> $(svars s)) (wrap x))
        else x
     |]
  | otherwise = [|id|]

-- | TH version of 'traceWith'
-- The argument is processed with 'svarsWith'.
-- Generated expression has type @forall r (a :: TYPE r) b a. (Show a, Rewrap a b) => a -> a@.
-- 'id' is generated if \"NOTRACE\" environment variable is defined.
tw :: String -> Q Exp
tw s
  | isTracingEnabled =
    [| \x ->
        if isTracingEnabled
        then unwrap (T.trace ($(traceMessage) <> $(svarsWith s) x) (wrap x))
        else x
     |]
  | otherwise = [|id|]

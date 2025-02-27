{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Tracing with TH
module Debug.TraceIf.TH
  ( TrMsgAndVars (..)
  , svars
  , tr
  , tw
  , svarsWith
  , traceMessage
  ) where

import Control.Lens hiding (levels)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class qualified as MT
import Data.Char as C
import Data.IORef
import Data.Generics.Labels ()
import Data.Text qualified as T
import Data.IntMap.Strict qualified as IM
import Data.RadixTree.Word8.Strict qualified as T
import qualified Debug.Trace as T
import Debug.TraceIf.If
import Debug.TraceIf.Show
import Debug.TraceIf.FileIndex
import Debug.TraceIf.Config hiding (traceMessage)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Prelude hiding (Show (..))
import Prelude qualified as P
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf


newtype TrMsgAndVars = TrMsgAndVars String deriving (Eq, P.Show)
newtype VarsPart = VarsPart String deriving (Eq, P.Show)
newtype ModTraceFlagVarName = ModTraceFlagVarName Name deriving (Eq, P.Show)

type SVarsFunM a = StateT (Maybe Name) Q a
type SVarsFun = TraceMessageFormat -> VarsPart -> SVarsFunM Exp

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
foo x y = trace $(svars "get/x y") x
@

The snippet above is expanded into:

@
foo x y = trace ("get; x: " <> show x <> "; y: " <> show y) x
@

'Show' instance of some types (eg lazy ByteString) hide
internal structure which might be important in low level code.
Variables after ";" are wrapped into t'ShowTrace':

@
import Data.ByteString.Lazy
foo x = trace $(svars "get/x;x") x
@

The snippet above is expanded into:

@
foo x = trace ("get; x: " <> show x <> "; x: " <> show (ShowTrace y)) x
@

-}
svars :: SVarsFun
svars tmf (VarsPart vars) = MT.lift $
  case span (';' /=) vars of
    (showVars, ';' : traceVars) ->
      [| $(listE (wordsToVars 'show showVars
                   <> wordsToVars 'showTrace traceVars)) :: [String] |]
    (showVars, "") ->
      [| $(listE (wordsToVars 'show showVars)) :: [String] |]
    (sv, st) -> do
      reportError $ printf "No case for %s %s" sv st
      [| [] |]
  where
    wordsToVars f vss = fmap go (varNamesFromPat vss)
      where
        go vs =
          lookupValueName vs >>= \case
            Nothing -> do
              reportError $ printf "no variable [%s]" vs
              [| $(lift vs) |]
            Just vn ->
              [| $(lift $ tmf ^. #entrySeparator)
                 <> $(lift vs)
                 <> $(lift $ tmf ^. #keyValueSeparator)
                 <> $(varE f) $(varE vn)
               |]

splitMessageFromVars :: TrMsgAndVars -> (String, VarsPart)
splitMessageFromVars (TrMsgAndVars trMsg) =
  case span ('/' /=) trMsg of
    (msgPart, '/':varPart) -> (msgPart, VarsPart varPart)
    (msgPart, []) -> (msgPart, VarsPart [])
    e ->  error $ "No case for:" <> show e

traceMessageLevel :: String -> (TraceLevel, TrMsgAndVars)
traceMessageLevel [] = (TracingDisabled, TrMsgAndVars [])
traceMessageLevel s@(l:m) = charToLevel l
  where
    charToLevel = \case
      '-' -> (Trace, TrMsgAndVars m)
      '!' -> (Warning, TrMsgAndVars m)
      '|' -> (Error, TrMsgAndVars m)
      _   -> (Info, TrMsgAndVars s)

-- | Suffix 'svars' with return value.
svarsWith :: SVarsFun
svarsWith tmf vp =
  get >>= maybe (calret (put . Just) =<< MT.lift (newName "retVal")) pure >>= \retValVarName -> MT.lift $
  [| $(evalStateT (svars tmf vp) Nothing)
     <> [ $(lift $ tmf ^. #retValPrefix)
        , show $(varE retValVarName)
        ]
     {- :: (Rewrap a b, Show a) => a -> [String] -}
   |]

concat2 :: Monoid m => [[m]] -> m
concat2 = mconcat . mconcat
{-# INLINE concat2 #-}

-- | Format whole trace message
traceMessage :: TrMsgAndVars -> TraceMessageFormat -> SVarsFun -> Q Exp
traceMessage mavs tmf svarsFun =
  runStateT itemExprs Nothing >>= \case
    (exprList, Nothing) ->
      [| concat2 $(pure $ ListE exprList) |]
    (exprList :: [Exp], Just retValVarName) ->
      [| \ $(varP retValVarName) -> concat2 $(pure $ ListE exprList) |]
  where
    itemExprs :: SVarsFunM [Exp]
    itemExprs = sequence (genItem <$> tmf ^. #traceLinePattern)
    loc = MT.lift location
    strL = ListE . (:[]) . LitE . StringL
    pStrL = pure . strL
    -- Exp :: String
    genItem :: TraceMessageElement -> SVarsFunM Exp
    genItem = \case
      LiteralMessage -> pStrL . fst $ splitMessageFromVars mavs
      Variables -> svarsFun tmf . snd $ splitMessageFromVars mavs
      FullyQualifiedModule -> strL . loc_module <$> loc
      ModuleName ->
        strL . reverse . takeWhile (/= '.') . reverse . loc_module <$> loc
      ShortenJavaModule -> do
        (eludom, htap) <- span (/= '.') . reverse . loc_module <$> loc
        pStrL $ shortenModPath True (reverse htap) <> (reverse eludom)
      PackageName -> strL . loc_package <$> loc
      FunctionName -> do
        lc <- loc
        let
          m = loc_module lc
          line = fst $ loc_start lc
        strL <$> MT.lift (fmap snd . IM.lookupLT line <$> getLineFileIndex lc >>= \case
          Nothing -> do
            reportWarning (printf "No function name for line [%d] in module [%s]" line m)
            pure "N/A"
          Just (FunName fn) -> pure fn)
      LineNumber -> strL . P.show . fst . loc_start <$> loc
      Delimiter del -> pStrL del

shortenModPath :: Bool -> String -> String
shortenModPath prevDot
  | prevDot = \case
      c : r -> c : shortenModPath False r
      [] -> []
  | otherwise = \case
      '.' : r -> '.' : shortenModPath True r
      _ : r -> shortenModPath False r
      [] -> []

flagVarName :: Q Name
flagVarName = do
  -- loc <- location
  newName "trace_if_flag_"
  -- <> show (reverse . takeWhile (/= '.') . reverse $ loc_module loc)

getModTraceFlagVar :: Q Name
getModTraceFlagVar =
  getQ >>= \case
    Just (ModTraceFlagVarName n) -> pure n
    Nothing -> do
      vn <- flagVarName
      putQ (ModTraceFlagVarName vn)
      nothingRefT <- [t| IORef (Maybe Bool) |]
      nothingRef <- [| unsafePerformIO (newIORef Nothing) |]
      addTopDecls
        [ SigD vn nothingRefT
        , ValD (VarP vn) (NormalB nothingRef) []
        ]
      pure vn

isLevelOverThreshold :: T.Lookup TraceLevel -> TraceLevel -> Bool
isLevelOverThreshold (T.Lookup _ levelThreshold) tl = levelThreshold <= tl
{-# INLINE isLevelOverThreshold #-}

-- | Eval level and cache
readTraceFlag :: T.Text -> TraceLevel -> String -> IORef (Maybe Bool) -> IO Bool
readTraceFlag modName trLvl evar fv = do
  readIORef fv >>= \case
    Just r -> pure r
    Nothing -> do
      T.lookupL T.Open (T.feedText modName) <$> getRuntimeConfig evar >>= \case
        Just threshold ->
          let !r = isLevelOverThreshold threshold trLvl in
            atomicWriteIORef fv (Just r) >> pure r
        Nothing ->
          atomicWriteIORef fv (Just False) >> pure False
{-# INLINE readTraceFlag #-}

traceG :: (TrMsgAndVars -> TraceMessageFormat -> Q Exp) -> String -> Q Exp
traceG genTraceLine s = do
  c <- getConfig
  case c ^. #mode of
    TraceDisabled -> [| \x -> x |]
    TraceStd ->
      case traceMessageLevel s of
        (TracingDisabled, _) -> [| \q -> q |]
        (tl, s') -> do
          modName <- T.pack . loc_module <$> location
          case T.lookupL T.Open (T.feedText modName) $ c ^. #levels of
            Nothing -> [| \x -> x |]
            Just threshold
              | isLevelOverThreshold threshold tl ->
                  case c ^. #runtimeLevelsOverrideEnvVar of
                    Ignored ->
                      genTraceLine s' $ c ^. #traceMessage
                    EnvironmentVariable evar -> do
                      vn <- getModTraceFlagVar
                      [| case unsafePerformIO (readTraceFlag modName tl evar $(varE vn)) of
                            True -> $(genTraceLine s' $ c ^. #traceMessage)
                            False -> \z -> z
                       |]
              | otherwise -> [| \y -> y |]

-- | TH version of 'trace'
-- The message is formatted according to 'TraceMessageFormat'.
-- The generated expression has type @forall r (a :: TYPE r) b a. Rewrap a b => a -> a@.
-- 'id' is generated if effective trace level is lower than trace level threshold.
-- Example:
--
-- > foo x = $(tr "get/x") x
--
tr :: String -> Q Exp
tr = traceG (\s fmt -> [| \x -> unwrap (T.trace $(traceMessage s fmt svars) (wrap x)) |])

-- | TH version of 'traceWith'
-- The message is formatted according to 'TraceMessageFormat'.
-- The generated expression has type @forall r (a :: TYPE r) b a. (Show a, Rewrap a b) => a -> a@.
-- 'id' is generated if \"NOTRACE\" environment variable is defined.
-- tw :: String -> Q Exp
tw :: String -> Q Exp
tw = traceG (\s fmt -> [| \x -> unwrap (T.trace ($(traceMessage s fmt svarsWith) x) (wrap x)) |])

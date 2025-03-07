{-# OPTIONS_HADDOCK hide #-}
module Debug.TraceEmbrace.Internal.TH where

import Control.DeepSeq
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
import Debug.TraceEmbrace.Config
import Debug.TraceEmbrace.FileIndex
import Debug.TraceEmbrace.Internal.Rewrap
import Debug.TraceEmbrace.Show
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Prelude hiding (Show (..))
import Prelude qualified as P
import Refined
import System.IO
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
              [| $(lift . unrefine $ tmf ^. #entrySeparator)
                 <> $(lift vs)
                 <> $(lift . unrefine $ tmf ^. #keyValueSeparator)
                 <> $(varE f) $(varE vn)
               |]

splitMessageFromVars :: TrMsgAndVars -> (String, VarsPart)
splitMessageFromVars (TrMsgAndVars trMsg) =
  case span ('/' /=) trMsg of
    (msgPart, '/':varPart) -> (msgPart, VarsPart varPart)
    (msgPart, []) -> (msgPart, VarsPart [])
    e ->  error $ "No case for:" <> show e

traceMessageLevel :: String -> (TraceLevel, TrMsgAndVars)
traceMessageLevel = fmap TrMsgAndVars . charToLevel

-- | Suffix 'svars' with return value.
svarsWith :: SVarsFun
svarsWith tmf vp =
  get >>= maybe (calret (put . Just) =<< MT.lift (newName "retVal")) pure >>= \retValVarName -> MT.lift $
  [| $(evalStateT (svars tmf vp) Nothing)
     <> [ $(lift . unrefine $ tmf ^. #retValPrefix)
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
    itemExprs = sequence (genItem <$> (unrefine (tmf ^. #traceLinePattern)))
    loc = MT.lift location
    strL = ListE . (:[]) . LitE . StringL
    pStrL = pure . strL
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
        strL <$> MT.lift (fmap snd . IM.lookupLE line <$> getLineFileIndex lc >>= \case
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
flagVarName =
  newName =<< ("_trace_if_flag_on_line_" <>) . show . fst . loc_start <$> location

getModTraceFlagVar :: Q Name
getModTraceFlagVar = do
  vn <- flagVarName
  putQ (ModTraceFlagVarName vn)
  nothingRefT <- [t| IORef (Maybe Bool) |]
  nothingRef <- [| unsafePerformIO (newIORef Nothing) |]
  addTopDecls
    [ SigD vn nothingRefT
    , ValD (VarP vn) (NormalB nothingRef) []
    , PragmaD (InlineP vn NoInline ConLike AllPhases)
    ]
  pure vn

isLevelOverThreshold :: T.Lookup TraceLevel -> TraceLevel -> Bool
isLevelOverThreshold (T.Lookup _ levelThreshold) tl = levelThreshold <= tl
{-# INLINE isLevelOverThreshold #-}

-- | Eval level and cache
readTraceFlag ::  T.Text -> TraceLevel -> DynConfigEnvVar -> IORef (Maybe Bool) -> IO Bool
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

traceG :: TraceEmbraceConfig -> Q Exp -> (TrMsgAndVars -> TraceMessageFormat -> Q Exp) -> String -> Q Exp
traceG c idF genTraceLine s =
  case c ^. #mode of
    TraceDisabled -> idF
    TraceStd -> go
    TraceUnsafeIo _ -> go
    TraceEvent -> go
  where
    go =
      case traceMessageLevel s of
        (TracingDisabled, _) -> idF
        (tl, s') -> do
          loc <- location
          let modName = T.pack $ loc_module loc
          case T.lookupL T.Open (T.feedText modName) $ c ^. #levels of
            Nothing -> idF
            Just threshold
              | isLevelOverThreshold threshold tl ->
                case envVarName loc (c ^. #runtimeLevelsOverrideEnvVar) of
                  Just evar -> do
                    vn <- getModTraceFlagVar
                    [| case unsafePerformIO (readTraceFlag modName tl evar $(varE vn)) of
                         True -> $(genTraceLine s' $ c ^. #traceMessage)
                         False -> $(idF)
                     |]
                  Nothing -> genTraceLine s' $ c ^. #traceMessage
              | otherwise -> idF

unsafePutStrLn :: IoSink -> String -> a -> a
unsafePutStrLn s msg v =
  msg `deepseq` (unsafePerformIO (hPutStrLn (getSinkHandle s) msg)) `seq` v
  where
{-# NOINLINE unsafePutStrLn #-}

getSinkHandle :: IoSink -> Handle
getSinkHandle s =
  case s of
    StdErrSink -> stderr
    StdOutSink -> stdout
    FileSink fp -> unsafePerformIO $ do
      readIORef unsafeIoSink >>= \case
        Just h -> pure h
        Nothing -> do
          nh <- openFile fp AppendMode
          (atomicModifyIORef' unsafeIoSink $ \case
            Nothing -> (Just nh, (False, nh))
            Just oh -> (Just oh, (True, oh))) >>= \case
               (True, h) ->  hClose nh >> pure h
               (False, h) -> pure h

safePutStrLn :: IoSink -> String -> a -> IO a
safePutStrLn s msg v =
  hPutStrLn (getSinkHandle s) msg >> pure v

chooseTraceFunOnTh :: Show s => TraceEmbraceConfig -> s -> Q Exp
chooseTraceFunOnTh c s =
  case c ^. #mode of
    TraceDisabled -> fail $ "Dead code on" <> show s
    TraceStd -> pure $ VarE 'T.trace
    TraceUnsafeIo snk -> [| unsafePutStrLn snk |]
    TraceEvent -> pure $ VarE 'T.traceEvent

tr :: Q Exp -> String -> Q Exp
tr idF rawMsg = do
  c <- getConfig
  traceG c idF (go c) rawMsg
  where
    go c s fmt =
      [| \x -> unwrap ($(chooseTraceFunOnTh c s) $(traceMessage s fmt svars) (wrap x)) |]

tw :: Q Exp -> String -> Q Exp
tw idF rawMsg = do
  c <- getConfig
  traceG c idF (go c) rawMsg
  where
    go c s fmt =
      [| \x -> unwrap ($(chooseTraceFunOnTh c s)
                        ($(traceMessage s fmt svarsWith) x)
                        (wrap x))
       |]

tw' :: Q Exp -> String -> Q Exp
tw' idF rawMsg = do
  c <- getConfig
  traceG c idF (go c) rawMsg
  where
    go c s fmt =
      [| \x -> unwrap ($(chooseTraceFunOnTh c s)
                        ($(traceMessage s fmt svarsWith) (ShowTrace x))
                        (wrap x))
       |]

trIo :: Q Exp -> String -> Q Exp
trIo idF rawMsg = do
  c <- getConfig
  traceG c idF (go c) rawMsg
  where
    go c s fmt = do
      let trFun = case c ^. #mode of
                    TraceDisabled -> error $ "Dead code on" <> show s
                    TraceStd -> 'T.traceIO
                    TraceUnsafeIo _ -> 'safePutStrLn
                    TraceEvent -> 'T.traceEventIO
      [| $(varE trFun) $(traceMessage s fmt svars) |]

trFunMarker :: Q Exp -> Q Exp
trFunMarker idF = do
  c <- getConfig
  let finalC = if c ^. #mode == TraceDisabled then c else markerConfig
  traceG finalC idF go "/"
  where
    go s fmt =
      [| \x -> unwrap (T.traceMarker $(traceMessage s fmt svars) (wrap x)) |]

trIoFunMarker :: Q Exp -> Q Exp
trIoFunMarker idF = do
  c <- getConfig
  let finalC = if c ^. #mode == TraceDisabled then c else markerConfig
  traceG finalC idF go "/"
  where
    go s fmt =
      [| T.traceMarkerIO $(traceMessage s fmt svars) |]

{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Index functions and methods of a Haskell module by line number
module Debug.TraceEmbrace.FileIndex where

import Data.IntMap.Strict qualified as IM
import Data.String ( IsString )
import GHC.Data.FastString ( mkFastString )
import GHC.Data.StringBuffer ( stringToStringBuffer )
import GHC.Driver.Config.Parser ( initParserOpts )
import GHC.Driver.DynFlags ( HasDynFlags(..) )
import GHC.Exts (IsList (toList))
import GHC.Hs.Extension (GhcPs)
import GHC.Parser ( parseModule )
import GHC.Parser.Lexer
    ( ParserOpts,
      P(unP),
      ParseResult(..),
      getPsErrorMessages,
      initParserState )
import GHC.Tc.Types ( TcM )
import GHC.Types.Name ( occNameString )
import GHC.Types.Name.Reader ( RdrName(Unqual) )
import GHC.Types.SrcLoc
    ( GenLocated(L), mkRealSrcLoc, realSrcSpanStart, srcLocLine )
import GHC.Utils.Outputable
    ( Outputable(ppr), defaultSDocContext, renderWithContext )
import Language.Haskell.Syntax
import Language.Haskell.TH.Syntax (Q (..), runIO, getQ, putQ, Loc (..), Lift, reportWarning)
import Language.Preprocessor.Cpphs (runCpphs, defaultCpphsOptions)
import Unsafe.Coerce ( unsafeCoerce )

#if MIN_VERSION_base(4,21,0)
import GHC.Parser.Annotation (epaLocationRealSrcSpan, SrcSpanAnnA, EpAnn(entry, EpAnn) )
#else
import GHC.Parser.Annotation (anchor, SrcSpanAnnA, EpAnn(entry, EpAnn))
import GHC.Types.SrcLoc (EpaLocation', RealSrcSpan)
epaLocationRealSrcSpan :: EpaLocation' a -> RealSrcSpan
epaLocationRealSrcSpan = anchor
#endif

newtype FunName = FunName { unFunName :: String } deriving (Show, Eq, Ord, IsString, Lift)
type LineFileIndex = IM.IntMap FunName

unsafeRunTcM :: TcM a -> Q a
unsafeRunTcM m = Q (unsafeCoerce m)

instance HasDynFlags Q where
  getDynFlags = unsafeRunTcM getDynFlags

calret :: Monad m => (a -> m ()) -> a -> m a
calret f a = f a >> pure a

getLineFileIndex' :: FilePath -> Q LineFileIndex
getLineFileIndex' fp = getQ >>= maybe (calret putQ =<< mkLineFunIndex fp) pure

getLineFileIndex :: Loc -> Q LineFileIndex
getLineFileIndex = getLineFileIndex' . loc_filename

indexEntry :: EpAnn ann -> GenLocated l RdrName -> [(Int, FunName)]
indexEntry EpAnn {entry} = \case
  L _ fi ->
    case fi of
      Unqual s ->
          [ ( srcLocLine (realSrcSpanStart (epaLocationRealSrcSpan entry))
            , FunName $ occNameString s
            )
          ]
      _ -> []

mkLineFunIndex :: FilePath -> Q LineFileIndex
mkLineFunIndex fp = do
  fileContent <- runIO (runCpphs defaultCpphsOptions fp =<< readFile fp)
  ops <- initParserOpts <$> getDynFlags
  case runParser fp ops fileContent parseModule of
    POk _ (L _ r) ->
      pure $ IM.fromList (concatMap extract (hsmodDecls r))
    PFailed ps -> do
      reportWarning $
        "Failed to parse [" <> fp <> "] for line function index due: " <>
        renderWithContext defaultSDocContext (ppr $ getPsErrorMessages ps) <>
        "\n--------------------------------------------------------------------"
      pure mempty
  where
    methodExtract (L l (FunBind {fun_id})) = indexEntry l fun_id
    methodExtract _ = []
    extract :: GenLocated SrcSpanAnnA (HsDecl GhcPs) -> [(IM.Key, FunName)]
    extract (L l (ValD _ (FunBind {fun_id}))) = indexEntry l fun_id
    extract (L _ (InstD _ (ClsInstD {cid_inst}))) =
      case cid_inst of
        ClsInstDecl {cid_binds} -> concatMap methodExtract (toList cid_binds)
    extract (L _ (TyClD _ (ClassDecl {tcdMeths}))) =
      concatMap methodExtract (toList tcdMeths)
    extract _ = []

runParser :: FilePath -> ParserOpts -> String -> P a -> ParseResult a
runParser filename opts str parser = unP parser parseState
  where
    location' = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState = initParserState opts buffer location'

{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Tree of line number to
module Debug.TraceIf.FileIndex where

import Data.IntMap.Strict qualified as IM
import Data.String
import GHC.Data.Bag
import GHC.Data.FastString
import GHC.Data.StringBuffer
import GHC.Driver.Config.Parser
import GHC.Driver.DynFlags
import GHC.Parser
import GHC.Parser.Annotation
import GHC.Parser.Lexer hiding (buffer)
import GHC.Tc.Types
import GHC.Types.Name hiding (Name)
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import Language.Haskell.Syntax
import Language.Haskell.TH.Syntax (Q (..), runIO, getQ, putQ, Loc (..), Lift)
import Unsafe.Coerce


newtype FunName = FunName String deriving (Show, Eq, Ord, IsString, Lift)
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

mkLineFunIndex :: FilePath -> Q LineFileIndex
mkLineFunIndex fp = do
  fileContent <- runIO (readFile fp)
  ops <- initParserOpts <$> getDynFlags
  case runParser ops fileContent parseModule of
    POk _ (L _ r) ->
      pure $ IM.fromList (concatMap extract (hsmodDecls r))
    _ -> fail $ "Parser failed for file: " <> show fp
  where
    indexEntry EpAnn {entry} = \case
      L _ fi ->
        case fi of
          Unqual s ->
              [ ( srcLocLine (realSrcSpanStart (anchor entry))
                , FunName $ occNameString s
                )
              ]
          _ -> []

    methodExtract (L l (FunBind {fun_id})) = indexEntry l fun_id
    methodExtract _ = []

    extract (L l (ValD _ (FunBind {fun_id}))) = indexEntry l fun_id
    extract (L _ (InstD _ (ClsInstD {cid_inst}))) =
      case cid_inst of
        ClsInstDecl {cid_binds} -> concatMap methodExtract (bagToList cid_binds)
        _ -> []
    extract (L _ (TyClD _ (ClassDecl {tcdMeths}))) =
      concatMap methodExtract (bagToList tcdMeths)
    extract _ = []

runParser :: ParserOpts -> String -> P a -> ParseResult a
runParser opts str parser = unP parser parseState
  where
    filename = "<interactive>"
    location' = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState = initParserState opts buffer location'

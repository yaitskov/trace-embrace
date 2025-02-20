{-# LANGUAGE TemplateHaskell #-}
module Debug.TraceIf.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Printf

-- | Interpolate vars in the arugment, prepend with location
-- Generate expression type is 'String'.
-- The argument has literal and interpolated parts.
-- There parts are separated with right slash (/)
svars :: String -> Q Exp
svars s = do
  l :: String <- locToStr literalPart <$> location
  case interpolatePart of
    '/':vars ->
         [|mconcat (l : $(listE (wordsToVars (words vars)))) :: String|]
    _ ->
      fail $ printf "Interpolation part is empty in: [%s]" s
  where
    locToStr lpart l = printf "%3d:%s %s" (fst $ loc_start l) (loc_module l) lpart
    (literalPart, interpolatePart) = span ('/' /=) s
    wordsToVars = fmap go
      where
        go vs =
          lookupValueName vs >>= \case
            Nothing -> do
              reportError $ printf "no variable [%s]" vs
              [| $(lift vs) |]
            Just vn ->
              [|"; " <> $(lift vs) <> ": " <> show $(varE vn)|]

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Debug.TraceIf.ShowTh where

import GHC.Exts
import Language.Haskell.TH
import Prelude hiding (Show (..))

-- | Levity polymorphic version 'P.Show'.
class Show (t :: TYPE r) where
  show :: t -> String

-- | https://gitlab.haskell.org/ghc/ghc/-/issues/25776
deriveShowTuple1 :: Name -> Q [Dec]
deriveShowTuple1 a = [d|
  instance Show (# $(conT a) #) where
    show (# a# #) = "(# " <> show a# <> " #)"
  |]

deriveShowTuple2 :: Name -> Name -> Q [Dec]
deriveShowTuple2 a b = [d|
  instance Show (# $(conT a), $(conT b) #) where
    show (# a#, b# #) = "(# " <> show a# <> ", " <> show b# <> " #)"
  |]

deriveShowTuple3 :: Name -> Name -> Name -> Q [Dec]
deriveShowTuple3 a b c = [d|
  instance Show (# $(conT a), $(conT b), $(conT c) #) where
    show (# a#, b#, c# #) = "(# " <> show a# <> ", " <> show b# <> ", " <> show c# <> " #)"
  |]

deriveShowTuple4 :: Name -> Name -> Name -> Name -> Q [Dec]
deriveShowTuple4 a b c d = [d|
  instance Show (# $(conT a), $(conT b), $(conT c), $(conT d) #) where
    show (# a#, b#, c#, d# #) = "(# " <> show a# <> ", " <> show b# <> ", " <> show c# <> ", " <> show d# <> " #)"
  |]

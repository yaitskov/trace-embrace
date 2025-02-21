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

show1 :: Name -> Q [Dec]
show1 a = [d|
  instance Show (# $(conT a) #) where
    show (# a# #) = "(# " <> show a# <> " #)"
  |]

show2 :: Name -> Name -> Q [Dec]
show2 a b = [d|
  instance Show (# $(conT a), $(conT b) #) where
    show (# a#, b# #) = "(# " <> show a# <> ", " <> show b# <> " #)"
  |]

show3 :: Name -> Name -> Name -> Q [Dec]
show3 a b c = [d|
  instance Show (# $(conT a), $(conT b), $(conT c) #) where
    show (# a#, b#, c# #) = "(# " <> show a# <> ", " <> show b# <> ", " <> show c# <> " #)"
  |]

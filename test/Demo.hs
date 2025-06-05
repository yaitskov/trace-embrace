{-# LANGUAGE CPP #-}
-- | File combines samples which are important for FileIndex
-- such us top functions, method instances and default methods
module Demo where

#if !MIN_VERSION_base(4,8,0)
#endif



(***) :: Int -> Int -> Int
x *** y = x * y

data X = X

instance Show X where
  show _ = "X"

class MyLen x where
  (+++) :: x -> x -> Int
  mylen :: x -> Int
  default mylen :: Show x => x -> Int
  mylen = length . show

instance MyLen Int where
  x +++ y = x + y
  mylen = id

foo :: Int -> Int
foo x = y
  where
    y = x

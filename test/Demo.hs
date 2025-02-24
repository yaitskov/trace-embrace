{-# LANGUAGE DefaultSignatures #-}
-- | File combines samples which are important for FileIndex
-- such us top functions, method instances and default methods
module Demo where

foo :: Int -> Int
foo x = y
  where
    y = x

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

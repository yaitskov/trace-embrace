# Haskell package: trace-if

The package minimizes the hassle of writing and maintaining traces in codebase.

There are several issues with functions from standand GHC module
[Debug.Trace](https://hackage.haskell.org/package/base/docs/Debug-Trace.html):

  * no trace emitting location
  * tracing multiple values requires to write boilerplate code
  * no way to quickly disable tracing without recompilation
  * no way to keep tracing in source code withou affecting production
    code performance

## Trace control

"NOTRACE" environment variable disables traces at runtime and
removes TH-driven tracing code completely at compile time.

## Examples

### TH version of traceWith

```haskell
{-# LANGUAGE TemplateHaskell #-}
module Foo where

import Debug.TraceIf

foo :: Int -> Int -> Int -> Int
foo x y z = $(tw "foo get/x y z") (x + y + z)
```

A trace line for the snippet above would be:

>   7:Foo foo get; x: 1; y: 2; z: 3 => 6

### traceWith without TH

```haskell
module Foo where

import Debug.TraceIf.If

foo :: Int -> Int -> Int -> Int
foo x y z =
  traceWith ((("foo get; x: " <> show x <>
                    "; y: " <> show y <>
                    "; z: " <> show z <>
                    " => ") <>) . show)
    (x + y + z)
```

### trace lazy ByteString structure

ByteString Show instance does not show chunks, but it can be important
in parser debugging. Value of a type with not enough informative Show
instance could be wrapped into 'ShowTrace' and more detailed Show
instance should be provided.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Foo where

import Debug.TraceIf
import Data.ByteString.Lazy

-- instance Show (ShowTrace ByteString) where
--   show ...

foo :: ByteString -> ByteString
foo bs = $(tr "foo get/bs#bs") bs
```

A trace line for the snippet above would be:

>  11:Foo foo get; bs: "abc"; bs: ["ab", "c"]


### Pattern matching syntax

Template tracing functions support Haskell pattern syntax and comments, so
function arguments can be quickly copy-pasted as-is:

```haskell
{-# LANGUAGE TemplateHaskell #-}
module Foo where

import Debug.TraceIf

foo :: Maybe ([Int], Int) -> Int
foo v@(Just ([x], {-ignore-} _)) = $(tr "foo get/v@(Just ([x], {-ignore-} _))") x
foo _ = 0
```

A trace line for the snippet above would be:

>   7:Foo foo get; v: Just 1; x: 1

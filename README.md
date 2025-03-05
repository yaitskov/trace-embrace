# [Embrace the trace](https://en.wiktionary.org/wiki/embrace_the_suck)

Writing tracing code is very boring activity, especially in Haskell.
The [trace-embrace](https://hackage.haskell.org/package/trace-embrace)
package minimizes the hassle of writing traces and maintaining them in
codebase. Thanks to TH-driven DSL whole chunks of code containing
function arguments could be quickly copy-pasted for tracing without
massaging in a text editor.

There are several issues with functions from standand GHC module
[Debug.Trace](https://hackage.haskell.org/package/base/docs/Debug-Trace.html):

  * no trace emitting location
  * tracing expressions solicit to write lot of boilerplate code
  * not possible to disable tracing without recompilation
  * tracing is coupled with optimizaiton flag
  * no per module granularity - all trace messages are disabled all at once

Let's look how trace-embrace deals with these issues.

## Location

TH macros with help of GHC lib, besides the module and the line of
exrpession emitting a trace message, find function or method name
where the expression is.

The trace message format is customizable through a config file
(`trace-embrace.yaml`) located next to a cabal one, which is
automatically generated if it is missing at build time. The trace line
pattern can include location related fields such as
[PackageName](https://hackage.haskell.org/package/trace-embrace/docs/Debug-TraceEmbrace.html#v:PackageName),
[FullyQualifiedModule](https://hackage.haskell.org/package/trace-embrace/docs/Debug-TraceEmbrace.html#v:FullyQualifiedModule),
[ModuleName](https://hackage.haskell.org/package/trace-embrace/docs/Debug-TraceEmbrace.html#v:ModuleName),
[FunctionName](https://hackage.haskell.org/package/trace-embrace/docs/Debug-TraceEmbrace.html#v:FunctionName),
and
[LineNumber](https://hackage.haskell.org/package/trace-embrace/docs/Debug-TraceEmbrace.html#v:LineNumber).

``` yaml
traceMessage:
  traceLinePattern:
  - tag: FullyQualifiedModule
  - contents: ':'
    tag: Delimiter
  - tag: FunctionName
  - contents: ' '
    tag: Delimiter
  - tag: LiteralMessage
  - tag: Variables
```

In a small function, a trace expression, containing only a space
separated list of variables, is still very informative, because the
rest is done by the library based on the expression context and
configuration. The function name gives literal part for tracing. The
library understands Haskell syntax very well and variables can be
copy/pasted in bulk "AS-IS" with comments and even pattern matching.

``` haskell
module Module where
fun x (Just y) = $(tr "/x (Just y)") $ x + y
```

The expression and config from above following trace message is
produced:

```
Module:fun ; x: 123; y: 777
```

The argument of `tr` consists of literal message and list of variables
for tracing. These parts are split by right slash.

## Trace control

Trace control has several dimensions.

### Compile/run time

Tracing code generation can be disabled at compile time in the [config
file](#configuration-file) or later at launch runtime via an environment
variable. The variable name depends on configuration and by default it
is a cabal package name (in upper case) prefixed with
`TRACE_EMBRACE_`.

``` yaml
mode:
  # expand all tracing macros to 'id' or 'pure ()' depending on the context
  tag: TraceDisabled
```
---

``` yaml
mode:
  # use Debug.Trace.trace group of functions
  tag: TraceStd
runtimeLevelsOverrideEnvVar:
  # disable runtime configuration
  tag: Ignored
```
---

If the environment variable is not defined then tracing is enabled.
If the variable expands to a dash (`-`) then tracing is disabled.
Otherwise the variable should contain a path to a file with module
prefixes specifing trace levels. Structure of runtime config file is
equal to the structure of `levels` section.

``` yaml
levels:
- '!Data.Map.Strict'     # exclamation mark is warning level
- '|Control.Concurrent'  # bar - bottom -> is error level
- 'Foo.Bar'              # default is info level
- '-'                    # dash is trace level
mode:
  tag: TraceStd
runtimeLevelsOverrideEnvVar:
  tag: CapsPackageName   # default
```
---

### Tracing levels

Both Haskell modules and tracing expressions have tracing levels.  If
expression tracing level is greater or equal to thershold tracing
level of containing module then the message is emitted. Modules by
default have threshold trace and unprefixed literal message has
tracing level info.

``` haskell
module Module where
yes x = $(tr "!I am emitted/") x
yep x $(tr "|I am emitted/") x
no x = $(tr "I am not emitted/") x
nope x = $(tr "-I am not emitted/") x
```
---

``` yaml
levels:
- '!Foo'
- '-Fo'
- '#Foo.Bar'
```

Runtime tracing level for a module cannot relax compile time tracing
level.

Every cabal package uses a dedicated envirnonment variable so no conflict
between dependencies using trace-embarce library is likely possible.

### Trace Sink

Besides
[Debug.Trace.trace](https://hackage.haskell.org/package/base/docs/Debug-Trace.html#v:trace)
and `/dev/null`,
[trIo](https://hackage.haskell.org/package/trace-embrace/docs/Debug-TraceEmbrace.html#v:trIo),
[tr](https://hackage.haskell.org/package/trace-embrace/docs/Debug-TraceEmbrace.html#v:tr)
[tw](https://hackage.haskell.org/package/trace-embrace/docs/Debug-TraceEmbrace.html#v:tw)
and
[tw'](https://hackage.haskell.org/package/trace-embrace/docs/Debug-TraceEmbrace.html#v:tw')
functions can forward tracing messages to
[hPutStrLn](https://hackage.haskell.org/package/base/docs/System-IO.html#v:hPutStrLn)
or
[Debug.Trace.traceEvent](https://hackage.haskell.org/package/base/docs/Debug-Trace.html#v:traceEvent).

``` yaml
mode:
  tag: TraceEvent
```

---

``` yaml
mode:
  sink:
    contents: /tmp/log.log
    tag: FileSink
  tag: TraceUnsafeIo
```
---

``` yaml
mode:
  sink:
    tag: StdErrSink
  tag: TraceUnsafeIo
```

## <a name="configuration-file"></a> Configuration file

The file is generanted on build if missing.

### Default compile time config file (trace-embrace.yaml)

``` yaml
levels:
- '-'
mode:
  tag: TraceStd
runtimeLevelsOverrideEnvVar:
  tag: CapsPackageName
traceMessage:
  entrySeparator: '; '
  keyValueSeparator: ': '
  retValPrefix: ' => '
  traceLinePattern:
  - tag: FullyQualifiedModule
  - contents: '::'
    tag: Delimiter
  - tag: FunctionName
  - contents: ': '
    tag: Delimiter
  - tag: LiteralMessage
  - tag: Variables
version: 1
```

### Sample of runtime config file


Runtime config file is also in YAML format, but its structure is way simpler.

``` yaml
- '-'         # empty prefix set default thershold equal to trace level
- '!Foo'
- 'Fo'
- '#Foo.Bar'  # threshold higher than error - disable tracing expression
```

Passing runtime config to `foo-bar.cabal`:


``` shell
TRACE_EMBRACE_FOO_BAR=- ./foo        # disable tracing
TRACE_EMBRACE_FOO_BAR=rtc.yaml ./foo # override threshold levels
```

## Examples

### TH version of traceWith

```haskell
{-# LANGUAGE TemplateHaskell #-}
module Module where

import Debug.TraceEmbrace

fun :: Int -> Int -> Int -> Int
fun x y z = $(tw "get/x y z") (x + y + z)
```

A trace line for the snippet above would be:

> Module:fun:  7 get; x: 1; y: 2; z: 3 => 6

### Trace lazy ByteString structure

[ByteString](https://hackage.haskell.org/package/bytestring/docs/Data-ByteString-Lazy.html#t:ByteString)
`Show` instance does not show chunks, but it can be important in
parser debugging (attoparsec). Value of a type with not enough
informative `Show` instance could be wrapped into
[ShowTrace](https://hackage.haskell.org/package/trace-embrace/docs/Debug-TraceEmbrace.html#t:ShowTrace)
and more detailed `Show` instance should be provided.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Module where

import Debug.TraceEmbrace
import Data.ByteString.Lazy

-- instance Show (ShowTrace ByteString) where
--   show ...

fun :: ByteString -> ByteString
fun bs = $(tr "get/bs;bs") bs
```

A trace line for the snippet above would be:

> Module:fun: 11 get; bs: "abc"; bs: ["ab", "c"]

For tracing returning values wrapped into
[ShowTrace](https://hackage.haskell.org/package/trace-embrace/docs/Debug-TraceEmbrace.html#t:ShowTrace)
use [tw'](https://hackage.haskell.org/package/trace-embrace/docs/Debug-TraceEmbrace.html#v:tw').

### Pattern matching syntax

Template tracing functions support Haskell pattern syntax and comments, so
function arguments can be quickly copy-pasted as-is:

```haskell
{-# LANGUAGE TemplateHaskell #-}
module Module where

import Debug.TraceEmbrace

fun :: Maybe ([Int], Int) -> Int
fun v@(Just ([x], {-ignore-} _)) = $(tr "get/v@(Just ([x], {-ignore-} _))") x
fun _ = 0
```

A trace line for the snippet above would be:

> Module:fun:  7 get; v: Just 1; x: 1

### Unlifted vars

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
module Module where

import Debug.TraceEmbrace
import GHC.Exts

fun :: Int -> Int
fun (I# x#) = (I# ($(tr "get/x#") x#))
```

A trace line for the snippet above would be:

> Module:fun:  7 get; x#: 1#

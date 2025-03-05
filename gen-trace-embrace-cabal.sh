#!/usr/bin/env bash

sed  "/^description:/bx ; b ; :x ; n ; e pandoc --to=haddock README.md | sed -E -e 's/^/    /' -e '/[Ee]mbrace[- ]the[- ]trace/d' ;" \
    trace-embrace.cabal.template > trace-embrace.cabal

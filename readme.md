numhask-bench
=============

[![Build
Status](https://travis-ci.org/tonyday567/numhask-bench.png)](https://travis-ci.org/tonyday567/numhask-bench)

See [results.md](results.md) for current performance numbers.

recipe
------

    stack build --exec "$(stack path --local-install-root)/bin/numhask-bench"

solo:

    stack exec "ghc" -- -O2 -rtsopts bench/mmult.lhs
    ./bench/mmult +RTS -s -RTS --runs 1000

profiling:

    stack build --profile

reference
---------

-   [perf](https://hackage.haskell.org/package/perf)
-   [numhask](https://hackage.haskell.org/package/numhask)


Current Performance
-------------------

dot

| run                   |       2|     100|
|:----------------------|-------:|-------:|
| NumHask.Array.Fixed   |  2.50e6|  1.26e6|
| NumHask.Array.Dynamic |  2.33e6|  3.12e6|
| vector                |  4.64e3|  3.42e3|

mmult

| run                     |      10|
|:------------------------|-------:|
| NumHask.Array.Fixed     |  3.88e4|
| NumHask.Array.Dynamic   |  5.12e4|
| NumHask.Array.HMatrix   |  1.11e3|
| Numeric.LinearAlgebra.R |  1.11e3|

numhask-bench
=============

[![Build
Status](https://travis-ci.org/tonyday567/numhask-bench.png)](https://travis-ci.org/tonyday567/numhask-bench)

recipe
------

    stack build --exec "$(stack path --local-install-root)/bin/numhask-bench"

reference
---------

-   [perf](https://hackage.haskell.org/package/perf)
-   [numhask-array](https://hackage.haskell.org/package/numhask-array)
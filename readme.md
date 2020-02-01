Current Performance
-------------------

dot

| run                   |       2|     100|
|:----------------------|-------:|-------:|
| NumHask.Array.Fixed   |  3.26e6|  1.04e6|
| NumHask.Array.Dynamic |  2.84e6|  3.08e6|
| vector                |  4.12e3|  3.50e3|

mmult

| run                     |      10|
|:------------------------|-------:|
| NumHask.Array.Fixed     |  3.83e4|
| NumHask.Array.Dynamic   |  5.08e4|
| NumHask.Array.HMatrix   |  1.17e3|
| Numeric.LinearAlgebra.R |  1.15e3|

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
-   <a href="https://stackoverflow.com/questions/11768656/reasonably-efficient-pure-functional-matrix-product-in-haskell" class="uri">https://stackoverflow.com/questions/11768656/reasonably-efficient-pure-functional-matrix-product-in-haskell</a>
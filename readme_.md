Current Performance
---

mmult 10x10
hmatrix 1115.00
Fixed 66786.92
HMatrix 1627.10
Dynamic 108337.39

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
-   https://stackoverflow.com/questions/11768656/reasonably-efficient-pure-functional-matrix-product-in-haskell

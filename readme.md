Current Performance
-------------------

mmult 10x10
hmatrix 1500.89
Fixed 2183.41
HMatrix 1521.00
Dynamic 2531.00

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

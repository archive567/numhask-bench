numhask-bench
=============

[![Build
Status](https://travis-ci.org/tonyday567/numhask-bench.png)](https://travis-ci.org/tonyday567/numhask-bench)

array performance
-----------------

The first runs compares creation of a 10x10 matrix and matrix
multiplication for:

-   A NumHask.Array instance with list as a container
-   A NumHask.Array instance with a boxed vector instance
-   [hmatrix](http://hackage.haskell.org/package/hmatrix)
-   [dense-linear-algebra](http://hackage.haskell.org/package/dense-linear-algebra)

<!-- -->

    square matrix size: 10

    mmult
    run                        first      2nd      3rd   median      av.

    numhask []                3.35e6   6.50e6   1.66e6   1.13e6   1.36e6
    numhask Boxed             1.04e6   1.56e6   1.84e6   1.30e6   1.44e6
    hmatrix                   1.88e4   2.63e3   1.99e3   1.90e3   5.09e3
    DLA                       2.32e5   9.45e4   5.36e4   5.39e4   8.30e4

    random version square matrix size: 10

    mmult, randoms
    run                        first      2nd      3rd   median      av.

    numhask []                1.15e6   1.45e6   1.07e6   1.11e6   1.22e6
    numhask Vector            1.33e6   1.28e6   1.08e6   1.20e6   1.36e6
    HMatrix                   6.49e5   6.41e3   9.32e3   2.07e3   9.09e3
    DLA                       9.46e4   9.06e4   1.19e5   5.83e4   1.08e5

    vector inner product size: 10

    <.>, randoms
    run                        first      2nd      3rd   median      av.

    numhask []                7.36e4   6.86e3   6.10e3   5.11e3   1.46e4
    numhask Vector            2.27e4   6.32e3   5.70e3   4.76e3   8.77e3
    HMatrix                   1.48e4   1.61e3   1.50e3   1.22e3   1.35e3
    DLA (row by column)       3.30e3   7.40e2   7.28e2   6.76e2   7.22e2

All measurements are in cycles. See
[perf](https://hackage.haskell.org/package/perf) for what this is. The
runs show the first three measurements, then the median and average of
many runs.

NumHask.Array operations
------------------------

    square matrix size: 10
    run                        first      2nd      3rd   median      av.

    row                       9.15e3   3.92e2   3.42e2   3.01e2   4.11e2
    col                       1.04e4   2.25e3   2.13e3   2.10e3   6.34e3
    unsafeRow                 1.19e3   4.12e2   4.18e2   2.56e2   2.79e2
    unsafeCol                 3.83e3   2.40e3   1.99e3   2.76e3   2.76e3
    unsafeIndex               2.12e3   3.46e2   2.32e2   1.96e2   2.42e2
    concat                    4.20e4   1.96e4   1.89e4   1.75e4   3.30e4
    transpose                 1.99e3   6.62e2   6.68e2   6.43e2   6.52e2

recipe
------

    stack build --exec "$(stack path --local-install-root)/bin/numhask-bench" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/bench_.md -t markdown -o readme.md --filter pandoc-include --mathjax" --file-watch

reference
---------

-   [perf](https://hackage.haskell.org/package/perf)
-   [numhask](https://hackage.haskell.org/package/numhask)

testing
-------

    stack clean
    stack build --executable-profiling --library-profiling --ghc-options "-fprof-auto -rtsopts"

http://hackage.haskell.org/package/inspection-testing-0.4.1.2/docs/Test-Inspection.html

    bench/benchListSimplest +RTS -hc -i0.00001 -xt
    bench/benchListSimplest +RTS -p

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

    numhask []                1.20e4   1.10e2   7.20e1   6.92e1   5.24e2
    numhask Boxed             5.21e4   2.41e4   2.25e4   5.39e3   2.87e4
    hmatrix                   2.42e4   2.67e3   2.09e3   2.17e3   3.79e3
    DLA                       5.98e4   5.48e4   5.38e4   5.46e4   8.55e4

    random version square matrix size: 10

    mmult, randoms
    run                        first      2nd      3rd   median      av.

    numhask []                2.12e3   1.30e2   7.80e1   7.53e1   1.30e2
    numhask Vector            1.44e4   6.07e3   6.46e3   5.22e3   1.45e4
    HMatrix                   1.01e5   2.22e3   1.33e3   1.79e3   2.81e3
    DLA                       5.96e4   5.61e4   8.83e4   5.65e4   7.68e4

    vector inner product size: 10

    <.>, randoms
    run                        first      2nd      3rd   median      av.

    numhask []                1.52e4   5.83e3   5.49e3   4.53e3   7.65e3
    numhask Vector            1.51e4   5.52e3   4.98e3   4.62e3   5.11e3
    HMatrix                   1.16e4   2.21e3   1.01e3   1.26e3   3.74e3
    DLA (row by column)       2.37e3   7.56e2   6.94e2   6.52e2   7.15e2

All measurements are in cycles. See
[perf](https://hackage.haskell.org/package/perf) for what this is. The
runs show the first three measurements, then the median and average of
many runs.

NumHask.Array operations
------------------------

    square matrix size: 10
    run                        first      2nd      3rd   median      av.

    row                       6.72e3   4.80e2   3.34e2   2.93e2   3.67e2
    col                       4.94e2   9.00e1   3.80e1   3.71e1   4.95e1
    unsafeRow                 5.68e2   6.20e1   1.54e2   3.95e1   4.75e1
    unsafeCol                 4.44e2   7.60e1   1.28e2   3.68e1   4.75e1
    unsafeIndex               2.44e3   6.26e2   2.80e2   2.51e2   2.95e2
    concat                    2.53e4   8.61e3   7.96e3   7.17e3   1.41e4
    transpose                 2.88e2   5.60e1   2.60e1   2.20e1   2.51e1

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

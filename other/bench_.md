numhask-bench
===

[![Build Status](https://travis-ci.org/tonyday567/numhask-bench.png)](https://travis-ci.org/tonyday567/numhask-bench)

array performance
---

The first runs compares creation of a 10x10 matrix and matrix multiplication for:

-   A NumHask.Array instance with list as a container
-   A NumHask.Array instance with a boxed vector instance
-   [hmatrix](http://hackage.haskell.org/package/hmatrix)
-   [dense-linear-algebra](http://hackage.haskell.org/package/dense-linear-algebra)

```include
other/array.md
```

All measurements are in cycles. See [perf](https://hackage.haskell.org/package/perf) for what this is.  The runs show the first three measurements, then the median and average of many runs.

NumHask.Array operations
---

```include
other/ops.md
```

recipe
---

```
stack build --exec "$(stack path --local-install-root)/bin/numhask-bench" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/bench_.md -t markdown -o readme.md --filter pandoc-include --mathjax" --file-watch
```

reference
---

- [perf](https://hackage.haskell.org/package/perf)
- [numhask](https://hackage.haskell.org/package/numhask)


testing
---

```
stack clean
stack build --executable-profiling --library-profiling --ghc-options "-fprof-auto -rtsopts"
```

http://hackage.haskell.org/package/inspection-testing-0.4.1.2/docs/Test-Inspection.html

```
bench/benchListSimplest +RTS -hc -i0.00001 -xt
bench/benchListSimplest +RTS -p
```


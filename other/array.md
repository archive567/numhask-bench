
```
square matrix size: 10

mmult
run                        first      2nd      3rd   median      av.

numhask []                1.20e4   1.10e2   7.20e1   6.92e1   5.24e2
numhask Boxed             5.21e4   2.41e4   2.25e4   5.39e3   2.87e4
hmatrix                   2.42e4   2.67e3   2.09e3   2.17e3   3.79e3
DLA                       5.98e4   5.48e4   5.38e4   5.46e4   8.55e4
```

```
random version square matrix size: 10

mmult, randoms
run                        first      2nd      3rd   median      av.

numhask []                2.12e3   1.30e2   7.80e1   7.53e1   1.30e2
numhask Vector            1.44e4   6.07e3   6.46e3   5.22e3   1.45e4
HMatrix                   1.01e5   2.22e3   1.33e3   1.79e3   2.81e3
DLA                       5.96e4   5.61e4   8.83e4   5.65e4   7.68e4
```

```
vector inner product size: 10

<.>, randoms
run                        first      2nd      3rd   median      av.

numhask []                1.52e4   5.83e3   5.49e3   4.53e3   7.65e3
numhask Vector            1.51e4   5.52e3   4.98e3   4.62e3   5.11e3
HMatrix                   1.16e4   2.21e3   1.01e3   1.26e3   3.74e3
DLA (row by column)       2.37e3   7.56e2   6.94e2   6.52e2   7.15e2
```

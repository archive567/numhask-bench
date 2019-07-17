
```
square matrix size: 10

mmult
run                        first      2nd      3rd   median      av.

numhask []                3.35e6   6.50e6   1.66e6   1.13e6   1.36e6
numhask Boxed             1.04e6   1.56e6   1.84e6   1.30e6   1.44e6
hmatrix                   1.88e4   2.63e3   1.99e3   1.90e3   5.09e3
DLA                       2.32e5   9.45e4   5.36e4   5.39e4   8.30e4
```

```
random version square matrix size: 10

mmult, randoms
run                        first      2nd      3rd   median      av.

numhask []                1.15e6   1.45e6   1.07e6   1.11e6   1.22e6
numhask Vector            1.33e6   1.28e6   1.08e6   1.20e6   1.36e6
HMatrix                   6.49e5   6.41e3   9.32e3   2.07e3   9.09e3
DLA                       9.46e4   9.06e4   1.19e5   5.83e4   1.08e5
```

```
vector inner product size: 10

<.>, randoms
run                        first      2nd      3rd   median      av.

numhask []                7.36e4   6.86e3   6.10e3   5.11e3   1.46e4
numhask Vector            2.27e4   6.32e3   5.70e3   4.76e3   8.77e3
HMatrix                   1.48e4   1.61e3   1.50e3   1.22e3   1.35e3
DLA (row by column)       3.30e3   7.40e2   7.28e2   6.76e2   7.22e2
```

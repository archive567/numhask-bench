Current Development
-------------------

Vector Inner Product

results agree

| run |       2|      10|     100|    1000|
|:----|-------:|-------:|-------:|-------:|
| NAF |  2.31e3|  1.14e4|  1.28e6|  1.34e6|
| NAD |  1.85e2|  7.14e2|  6.85e3|  6.37e3|
| NAH |  5.15e2|  5.13e2|  7.21e2|  7.20e2|
| V   |  8.32e1|  3.66e2|  3.40e3|  3.37e3|
| H   |  5.81e2|  5.90e2|  6.98e2|  7.06e2|

Matrix Multiplication

| run                     |      10|
|:------------------------|-------:|
| NumHask.Array.Fixed     |  4.07e4|
| Numeric.LinearAlgebra.R |  1.14e3|
| Statistics.Matrix.Fast  |  5.28e3|
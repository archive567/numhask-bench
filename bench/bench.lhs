\begin{code}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import qualified Protolude as P
import NumHask.Array
import NumHask.Prelude
import Options.Generic
import Perf
import Perf.Analysis
import qualified Numeric.LinearAlgebra as H
import qualified Data.Vector as V
import qualified Statistics.Matrix as DLA
import System.Random.MWC
import Readme.Lhs

instance NFData DLA.Matrix where
  rnf m = seq m ()

oneRunList10 g f = do
  ra <- sequence $ replicate 100 (uniform g :: IO Double)
  rb <- sequence $ replicate 100 (uniform g :: IO Double)
  let !aa = fromList ra :: Array [] '[10, 10] Double
  let ab = fromList rb :: Array [] '[10, 10] Double
  (res, r) <- tickNoinline (f aa) ab
  pure (res, r)

oneRunVector10 g f = do
  ra <- sequence $ replicate 100 (uniform g :: IO Double)
  rb <- sequence $ replicate 100 (uniform g :: IO Double)
  let !aa = fromList ra :: Array V.Vector '[10, 10] Double
  let ab = fromList rb :: Array V.Vector '[10, 10] Double
  (res, r) <- tickNoinline (f aa) ab
  pure (res, r)

oneRunHMatrix g f sz = do
  ra <- sequence $ replicate (sz*sz) (uniform g :: IO Double)
  rb <- sequence $ replicate (sz*sz) (uniform g :: IO Double)
  let !aa = H.matrix sz ra
  let !ab = H.matrix sz rb
  (res, r) <- tickNoinline (f aa) ab
  pure (res, r)

oneRunDLA g f sz = do
  ra <- sequence $ replicate (sz*sz) (uniform g :: IO Double)
  rb <- sequence $ replicate (sz*sz) (uniform g :: IO Double)
  let !aa = DLA.fromList sz sz ra
  let !ab = DLA.fromList sz sz rb
  (res, r) <- tickNoinline (f aa) ab
  pure (res, r)

oneRunListV10 g f = do
  ra <- sequence $ replicate 10 (uniform g :: IO Double)
  rb <- sequence $ replicate 10 (uniform g :: IO Double)
  let !aa = fromList ra :: Array [] '[10] Double
  let !ab = fromList rb :: Array [] '[10] Double
  (res, r) <- tickNoinline (f aa) ab
  pure (res, r)

oneRunVectorV10 g f = do
  ra <- sequence $ replicate 10 (uniform g :: IO Double)
  rb <- sequence $ replicate 10 (uniform g :: IO Double)
  let !aa = fromList ra :: Array V.Vector '[10] Double
  let !ab = fromList rb :: Array V.Vector '[10] Double
  (res, r) <- tickNoinline (f aa) ab
  pure (res, r)

oneRunHMatrixV g f sz = do
  ra <- sequence $ replicate sz (uniform g :: IO Double)
  rb <- sequence $ replicate sz (uniform g :: IO Double)
  let !aa = H.vector ra
  let !ab = H.vector rb
  (res, r) <- tickNoinline (f aa) ab
  pure (res, r)

oneRunDLAV g f sz = do
  ra <- sequence $ replicate sz (uniform g :: IO Double)
  rb <- sequence $ replicate sz (uniform g :: IO Double)
  let !aa = DLA.fromList 1 sz ra
  let !ab = DLA.fromList sz 1 rb
  (res, r) <- tickNoinline (f aa) ab
  pure (res, r)

newtype Opts = Opts
  { runs :: Maybe Int -- <?> "number of runs"
  } deriving (Generic, Show)

instance ParseRecord Opts

main :: IO ()
main = do
  o :: Opts <- getRecord "benchmarking numhask array"
  let !n = fromMaybe 100 (runs o)
  _ <- warmup 100
  g <- create

  -- sz = 10 run
  let sz = 10 :: Int

  -- there is no way to avoid the hardcode :(
  let !aa = [1 ..] :: Array [] '[10, 10] Double
  let !ab = [0 ..] :: Array [] '[10, 10] Double

  let !va = [1 ..] :: Array V.Vector '[10, 10] Double
  let !vb = [0 ..] :: Array V.Vector '[10, 10] Double

  let !ha = (sz H.>< sz) [1 :: H.Z ..]
  let !hb = (sz H.>< sz) [1 :: H.Z ..]

  let !dlaa = DLA.fromList sz sz [1 .. (fromIntegral $ sz*sz)]
  let !dlab = DLA.fromList sz sz [0 .. (fromIntegral $ sz*sz - 1)]

  (rmmult, _) <- ticks n (NumHask.Array.mmult aa) ab
  (rmmulth, _) <- ticks n (ha H.<>) hb
  (rmmultv, _) <- ticks n (NumHask.Array.mmult va) vb
  (rmmultdla, _) <- ticks n (DLA.multiply dlaa) dlab


  xList <- sequence $ replicate n (oneRunList10 g mmult)
  print $ sum $ sum $ fmap snd xList
  xVec <- sequence $ replicate n (oneRunVector10 g mmult)
  print $ sum $ sum $ fmap snd xVec
  xH <- sequence $ replicate n (oneRunHMatrix g (H.<>) sz)
  print $ P.sum $ fmap P.sum $ fmap (fmap P.sum) $ fmap H.toLists $ fmap snd xH
  xDLA <- sequence $ replicate n (oneRunDLA g DLA.multiply sz)
  print $ sum $ fmap sum $ fmap (fmap sum) $ fmap DLA.toRowLists $ fmap snd xDLA

  xListV <- sequence $ replicate n (oneRunListV10 g (<.>))
  print $ sum $ fmap snd xList
  xVecV <- sequence $ replicate n (oneRunVectorV10 g (<.>))
  print $ sum $ fmap snd xVec
  xHV <- sequence $ replicate n (oneRunHMatrixV g (H.<.>) sz)
  print $ P.sum $ fmap snd xH
  xDLAV <- sequence $ replicate n (oneRunDLAV g DLA.multiply sz)
  print $ sum $ fmap sum $ fmap (fmap sum) $ fmap DLA.toRowLists $ fmap snd xDLA
  
  -- numhask operations
  (rrow, _) <- ticks n (NumHask.Array.row (Proxy :: Proxy 4)) ab
  (rcol, _) <- ticks n (NumHask.Array.col (Proxy :: Proxy 4)) ab
  (runsaferow, _) <- ticks n (NumHask.Array.unsafeRow 0) ab
  (runsafecol, _) <- ticks n (NumHask.Array.unsafeCol 0) ab
  (runsafeindex, _) <- ticks n (NumHask.Array.unsafeIndex ab) [2, 3]
  (rconcat, _) <- ticks n (concatenate (Proxy :: Proxy 2) aa) aa
  (rtranspose, _) <- ticks n NumHask.Array.transpose aa

  void $ runOutput
    ("bench/bench.lhs", LHS)
    ("bench.md", GitHubMarkdown) $ do

    output "mmult" $ Native $
      [ plain ("square matrix size: " <> show sz)
      ] <>
      [formatRuns 3 2
       [ ("numhask", rmmult)
       , ("numhask boxed", rmmultv)
       , ("hmatrix", rmmulth)
       , ("DLA", rmmultdla)
       ]
      ]

    output "random-mmult" $ Native $
      [ plain ("square matrix size: " <> show sz)
      ] <>
      [formatRuns 3 2
       [ ("numhask", fmap fst xList)
       , ("numhask boxed", fmap fst xVec)
       , ("hmatrix", fmap fst xH)
       , ("DLA", fmap fst xDLA)
       ]
      ]

    output "random-mmult-vector" $ Native $
      [ plain ("square matrix size: " <> show sz)
      ] <>
      [formatRuns 3 2
       [ ("numhask", fmap fst xListV)
       , ("numhask boxed", fmap fst xVecV)
       , ("hmatrix", fmap fst xHV)
       , ("DLA", fmap fst xDLAV)
       ]
      ]

    output "ops" $ Native $
      [ plain ("square matrix size: " <> show sz)
      ] <>
      [formatRuns 3 2
       [ ("row", rrow)
       , ("col", rcol)
       , ("unsafeRow", runsaferow)
       , ("unsafeCol", runsafecol)
       , ("unsafeIndex", runsafeindex)
       , ("unsafeIndex", runsafeindex)
       , ("concat", rconcat)
       , ("transpose", rtranspose)
       ]
      ]

  pure ()
\end{code}


results
===

array performance
-----------------

The first runs compares creation of a 10x10 matrix and matrix
multiplication for:

-   A NumHask.Array instance with list as a container
-   A NumHask.Array instance with a boxed vector instance
-   [hmatrix](http://hackage.haskell.org/package/hmatrix)
-   [dense-linear-algebra](http://hackage.haskell.org/package/dense-linear-algebra)

```{.output .mmult}
```

Using random numbers

```{.output .random-mmult}
```

inner product

```{.output .random-mmult-vector}
```

operations

```{.output .ops}
```

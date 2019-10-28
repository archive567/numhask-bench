
Current Development
---

Vector Inner Product

```{.output .inner}
```

Matrix Multiplication
```{.output .mmult}
```

code
===

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
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import qualified Protolude as P
import NumHask.Array.Simple as A
import NumHask.Vector as VNH
import NumHask.Prelude as NH
import Options.Generic
import Perf
import Perf.Analysis
import qualified Numeric.LinearAlgebra as H
import qualified Data.Vector as V
import Readme.Lhs
import qualified Statistics.Matrix.Fast as DLAF
import qualified Statistics.Matrix as DLA

-- | format a run median
formatMedian :: (P.Integral a) => Text -> [[a]] -> [Text]
formatMedian label xss =
  [label] <>
  ((formatF 2 . percentile 0.5) <$> xss)

formatRunsMedian :: (P.Integral a) => [Text] -> [(Text, [[a]])] -> Block
formatRunsMedian h rs =
  table
  mempty
  (["run"] <> h)
  ([AlignLeft] <> replicate n AlignRight)
  []
  (fmap (\(l,as) -> formatMedian l as) rs)
  where
    n = length h

newtype Opts = Opts
  { runs :: Maybe Int -- <?> "number of runs"
  } deriving (Generic, Show)

instance ParseRecord Opts

main :: IO ()
main = do
  o :: Opts <- getRecord "benchmarking numhask array"
  let !n = fromMaybe 100 (runs o)
  _ <- warmup 100

  let tdotv x = ticks n (sum . V.zipWith (*) x) x
  let tdota x = ticks n (x NH.<.>) x
  let tdoth x = ticks n (x H.<.>) x

  -- size = 2
  let !vv2 = V.fromList [1..2] :: V.Vector Double
  let !va2 = fromList [1 ..] :: VNH.Vector 2 Double
  let !vh2 = H.fromList [1 :: H.R .. 2]

  rv2 <- tdotv vv2
  ra2 <- tdota va2
  rh2 <- tdoth vh2

  -- size = 10
  let !vv10 = V.fromList [1..10] :: V.Vector Double
  let !va10 = fromList [1 ..] :: VNH.Vector 10 Double
  let !vh10 = H.fromList [1 :: H.R .. 10]

  rv10 <- tdotv vv10
  ra10 <- tdota va10
  rh10 <- tdoth vh10

  -- size = 100
  let !vv100 = V.fromList [1..100] :: V.Vector Double
  let !va100 = fromList [1 ..] :: VNH.Vector 100 Double
  let !va100' = fromList [1 ..] :: A.Array '[100] Double
  let !vh100 = H.fromList [1 :: H.R .. 100]

  rv100 <- tdotv vv100
  ra100 <- tdota va100
  ra100' <- tdota va100'
  rh100 <- tdoth vh100

  putStrLn ("dot product"::Text)
  putStrLn $
    bool
    ("mismatched results for dot" :: Text)
    "dot results are equal"
    (snd rv100 == snd ra100 && snd rv100 == snd rh100)
  putStrLn $ ("numhask " :: Text) <> formatF 2 (percentile 0.5 (fst ra100))
  putStrLn $ ("numhask' " :: Text) <> formatF 2 (percentile 0.5 (fst ra100'))
  putStrLn $ ("hmatrix " :: Text) <> formatF 2 (percentile 0.5 (fst rh100))

  let ma x = ticks n (x `mmult`) x
  let mh x = ticks n (x H.<>) x
  let md x = ticks n (DLAF.multiply x) x

  let !ma10 = [1 ..] :: A.Array '[10, 10] Double
  let !mh10 = (10 H.>< 10) [1 :: H.R ..]
  let !md10 = DLA.fromList 10 10 [1 .. (fromIntegral $ (10 :: Int) * 10)]

  rma10 <- ma ma10
  rmh10 <- mh mh10
  rmd10 <- md md10

  putStrLn ("mmult" :: Text)
  putStrLn $ ("numhask " :: Text) <> formatF 2 (percentile 0.5 (fst rma10))
  putStrLn $ ("hmatrix " :: Text) <> formatF 2 (percentile 0.5 (fst rmh10))
  putStrLn $ ("dla " :: Text) <> formatF 2 (percentile 0.5 (fst rmd10))

  void $ runOutput
    ("bench/bench.lhs", LHS)
    ("bench.md", GitHubMarkdown) $ do

    output "inner" $ Native
      [formatRunsMedian ["2", "10", "100"]
       [ ("vector", [fst rv2, fst rv10, fst rv100])
       , ("numhask vector", [fst ra2, fst ra10, fst ra100])
       , ("hmatrix", [fst rh2, fst rh10, fst rh100])
       ]
      ]

    output "mmult" $ Native
      [formatRunsMedian ["10"]
       [ ("numhask", [fst rma10])
       , ("hmatrix", [fst rmh10])
       , ("dla", [fst rmd10])
       ]
      ]

  pure ()
\end{code}

Current Development
-------------------

Vector Inner Product

| run                     |       2|      10|     100|
|:------------------------|-------:|-------:|-------:|
| Numeric.LinearAlgebra.R |  8.34e2|  8.21e2|  8.24e2|
| Data.Vector             |  8.66e1|  4.04e2|  9.12e3|
| NumHask.Array.Fixed     |  7.76e1|  8.83e1|  9.01e1|

Matrix Multiplication

| run                     |      10|
|:------------------------|-------:|
| NumHask.Array.Fixed     |  3.82e4|
| Numeric.LinearAlgebra.R |  1.58e3|
| Statistics.Matrix.Fast  |  5.50e3|

code
====

``` {.haskell}

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
import NumHask.Array.Fixed as NAF
import NumHask.Array.Dynamic as NAD
import qualified NumHask.Array.HMatrix as NAH
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
  let tdota x = ticks n ((dot sum (*)) x) x
  let tdoth x = ticks n (x H.<.>) x
  let tdotnah x = ticks n (x NH.<.>) x

  -- size = 2
  let !vv2 = V.fromList [1..2] :: V.Vector Double
  let !va2 = fromList [1 .. 2] :: NAF.Array '[2] Double
  let !vh2 = H.fromList [1 :: H.R .. 2]

  rv2 <- tdotv vv2
  ra2 <- tdota va2
  rh2 <- tdoth vh2

  -- size = 10
  let !vv10 = V.fromList [1..10] :: V.Vector Double
  let !va10 = fromList [1 .. 10] :: NAF.Array '[10] Double
  let !vh10 = H.fromList [1 :: H.R .. 10]

  rv10 <- tdotv vv10
  ra10 <- tdota va10
  rh10 <- tdoth vh10

  -- size = 100
  let !vv100 = V.fromList [1..100] :: V.Vector Double
  let !va100 = fromList [1 .. 100] :: NAF.Array '[100] Double
  let !vnah100 = fromList [1 .. 100] :: NAH.HArray '[100] Double
  let !vh100 = H.fromList [1 :: H.R .. 100]


  rv100 <- tdotv vv100
  ra100 <- tdota va100
  rh100 <- tdoth vh100
  rnah100 <- tdotnah vnah100

  putStrLn ("dot product n=100 "::Text)
  putStrLn $
    bool
    ("mismatched results for dot" :: Text)
    "dot results are equal"
    (snd rv100 == snd rv100 && snd rv100 == snd rh100)

  putStrLn $ ("Numeric.LinearAlgebra.R " :: Text) <> formatF 2 (percentile 0.5 (fst rh100))
  putStrLn $ ("Data.Vector " :: Text) <> formatF 2 (percentile 0.5 (fst rv100))
  putStrLn $ ("NumHask.Array.Simple " :: Text) <> formatF 2 (percentile 0.5 (fst ra100))
  putStrLn $ ("NumHask.Array.HMatrix " :: Text) <> formatF 2 (percentile 0.5 (fst rnah100))

  let ma x = ticks n (x `NAF.mmult`) x
  let mnad x = ticks n (x `NAD.mmult`) x
  let mnah x = ticks n (x `NAH.mmult`) x
  let mh x = ticks n (x H.<>) x
  let md x = ticks n (DLAF.multiply x) x

  let !ma10 = [1 .. 100] :: NAF.Array '[10, 10] Double
  let !mnah10 = [1 .. 100] :: NAH.HArray '[10, 10] Double
  let !mnad10 = fromFlatList [10,10] [1 .. 100] :: NAD.DArray Double
  let !mh10 = (10 H.>< 10) [1 :: H.R ..]
  let !md10 = DLA.fromList 10 10 [1 .. (fromIntegral $ (10 :: Int) * 10)]

  rma10 <- ma ma10
  rmh10 <- mh mh10
  rmd10 <- md md10
  rmnah10 <- mnah mnah10
  rmnad10 <- mnad mnad10

  putStrLn ("mmult 10x10" :: Text)
  putStrLn $ ("Numeric.LinearAlgebra.R " :: Text) <> formatF 2 (percentile 0.5 (fst rmh10))
  putStrLn $ ("NumHask.Array.Fixed " :: Text) <> formatF 2 (percentile 0.5 (fst rma10))
  putStrLn $ ("Statistics.Matrix.Fast " :: Text) <> formatF 2 (percentile 0.5 (fst rmd10))
  putStrLn $ ("NumHask.Array.HMatrix " :: Text) <> formatF 2 (percentile 0.5 (fst rmnah10))
  putStrLn $ ("NumHask.Array.Dynamic " :: Text) <> formatF 2 (percentile 0.5 (fst rmnad10))

  void $ runOutput
    ("bench/bench.lhs", LHS)
    ("bench.md", GitHubMarkdown) $ do

    output "inner" $ Native
      [formatRunsMedian ["2", "10", "100"]
       [ ("Numeric.LinearAlgebra.R", [fst rh2, fst rh10, fst rh100])
       , ("Data.Vector", [fst rv2, fst rv10, fst rv100])
       , ("NumHask.Array.Fixed", [fst ra2, fst ra10, fst ra100])
       ]
      ]

    output "mmult" $ Native
      [formatRunsMedian ["10"]
       [ ("NumHask.Array.Fixed", [fst rma10])
       , ("Numeric.LinearAlgebra.R", [fst rmh10])
       , ("Statistics.Matrix.Fast", [fst rmd10])
       ]
      ]

  pure ()
```

mmult testing
---
    stack clean
    stack build --profile
    stack exec "ghc" -- -O2 -rtsopts -prof bench/mmult_dla.lhs
    time bench/mmult_dla +RTS -p

Matrix Multiplication
```{.output .mmult}
```

code
===

\begin{code}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
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

import NumHask.Prelude as NH
import Perf
import Perf.Analysis
-- import qualified Statistics.Matrix as DLA
-- import qualified Statistics.Matrix.Fast as DLAF
import NumHask.Matrix as M
import NumHask.Matrix.Unboxed as U
import NumHask.Array.Simple as A

main :: IO ()
main = do
  let !n = 1000
  _ <- warmup 100

  --let mh x = ticks n (x H.<>) x
  --let mnh x = ticks n (x `mmult`) x
  let tu x = ticks n (x `U.mmult`) x
  let tb x = ticks n (x `M.mmult`) x
  let ta x = ticks n (x `A.mmult`) x
  let ta' x = ticks n (x `A.mmult'`) x
  -- let mdla x = ticks n (DLAF.multiply x) x

  --let !va10 = [1 ..] :: Array V.Vector '[10, 10] Double
  --let !ha10 = (10 H.>< 10) [1 :: H.R ..]
  let !u10 = [1 ..] :: U.Matrix 10 10 Double
  let !b10 = [1 ..] :: M.Matrix 10 10 Double
  let !a10 = [1 ..] :: A.Array [10, 10] Double
  --let !dla10 = DLA.fromList 10 10 [1 .. (fromIntegral $ (10 :: Int) * 10)]

  -- rmh10 <- mh ha10
  -- rmnha10 <- mnh va10
  ru10 <- tu u10
  rb10 <- tb b10
  ra10 <- ta a10
  ra10' <- ta' a10
  -- rmdla10 <- mdla dla10

  -- putStrLn $ ("numhask array: performance: " :: Text) <> show (percentile 0.5 $ fst rmnha10)
  -- putStrLn $ ("numhask array: result: " :: Text) <> show (sum $ snd rmnha10)
  putStrLn $ ("numhask matrix unboxed: performance: " :: Text) <> show (percentile 0.5 $ fst ru10)
  putStrLn $ ("numhask matrix boxed: performance: " :: Text) <> show (percentile 0.5 $ fst rb10)
  putStrLn $ ("numhask array boxed: performance: " :: Text) <> show (percentile 0.5 $ fst ra10)
  putStrLn $ ("numhask array: tester " :: Text) <> show (percentile 0.5 $ fst ra10')
  -- putStrLn $ ("numhask matrix: result: " :: Text) <> show (UV.foldr (+) NH.zero $ S.fromSized $ unUMatrix $ snd rmnh10)
  -- putStrLn $ ("hmatrix: performance: " :: Text) <> show (percentile 0.5 $ fst rmh10)
  -- putStrLn $ ("hmatrix: result: " :: Text) <> show (H.sumElements $ snd rmh10)
  -- putStrLn $ ("dla: performance: " :: Text) <> show (percentile 0.5 $ fst rmdla10)
  -- putStrLn $ ("dla: result: " :: Text) <> show (sum $ DLA.toList $ snd rmdla10)

  pure ()
\end{code}

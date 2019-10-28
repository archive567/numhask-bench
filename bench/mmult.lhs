
mmult testing
---
    stack clean
    stack build --profile
    stack exec "ghc" -- -O2 -rtsopts -prof bench/mmult.lhs
    time bench/mmult +RTS -p

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

import NumHask.Array hiding (Vector)
import NumHask.Prelude as NH
import Options.Generic
import Perf
import Perf.Analysis
import qualified Numeric.LinearAlgebra as H
import qualified Data.Vector as V 
import qualified NumHask.Matrix as M
import qualified Statistics.Matrix as DLA
import qualified Statistics.Matrix.Fast as DLAF

newtype Opts = Opts
  { runs :: Maybe Int -- <?> "number of runs"
  } deriving (Generic, Show)

instance ParseRecord Opts

main :: IO ()
main = do
  o :: Opts <- getRecord "benchmarking numhask array"
  let !n = fromMaybe 100 (runs o)
  _ <- warmup 100

  --let mh x = ticks n (x H.<>) x
  --let mnh x = ticks n (x `mmult`) x
  --let mm x = ticks n (x `M.mmult`) x
  let mdla x = ticks n (DLAF.multiply x) x

  --let !va10 = [1 ..] :: Array V.Vector '[10, 10] Double
  --let !ha10 = (10 H.>< 10) [1 :: H.R ..]
  --let !ma10 = [1 ..] :: M.Matrix 10 10 Double
  let !dla10 = DLA.fromList 10 10 [1 .. (fromIntegral $ (10 :: Int) * 10)]

  -- rmh10 <- mh ha10
  -- rmnha10 <- mnh va10
  -- rmnh10 <- mm ma10
  rmdla10 <- mdla dla10

  -- putStrLn $ ("numhask array: performance: " :: Text) <> show (percentile 0.5 $ fst rmnha10)
  -- putStrLn $ ("numhask array: result: " :: Text) <> show (sum $ snd rmnha10)
  -- putStrLn $ ("numhask matrix: performance: " :: Text) <> show (percentile 0.5 $ fst rmnh10)
  -- putStrLn $ ("numhask matrix: result: " :: Text) <> show (sum $ snd rmnh10)
  -- putStrLn $ ("hmatrix: performance: " :: Text) <> show (percentile 0.5 $ fst rmh10)
  -- putStrLn $ ("hmatrix: result: " :: Text) <> show (H.sumElements $ snd rmh10)
  putStrLn $ ("dla: performance: " :: Text) <> show (percentile 0.5 $ fst rmdla10)
  putStrLn $ ("dla: result: " :: Text) <> show (sum $ DLA.toList $ snd rmdla10)

  pure ()
\end{code}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import NumHask.Array hiding (zipWith)
import NumHask.Prelude
-- import System.Random.MWC
import Criterion.Main
import qualified Numeric.LinearAlgebra as H
import qualified Data.Vector as V 
import qualified NumHask.Matrix as M
import qualified Statistics.Matrix as DLA
import qualified Statistics.Matrix.Fast as DLAF
-- import qualified Statistics.Matrix.Fast.Algorithms as A
import Perf.Criterion
import Perf.Analysis
import Perf
import Readme.Lhs
import Criterion.Measurement.Types (Measured, rescale, measCycles)

main :: IO ()
main = do
  let !va10 = [1 ..] :: Array V.Vector '[10, 10] Double
  let !ha10 = (10 H.>< 10) [1 :: H.R ..]
  let !ma10 = [1 ..] :: M.Matrix 10 10 Double
  let !dla10 = DLA.fromList 10 10 [1 .. (fromIntegral $ (10 :: Int) * 10)]
  defaultMain [
    bgroup "cri"
    [ bench "mmult array numhask"  $ nf (mmult va10) va10
    , bench "mmult matrix numhask"  $ nf (M.mmult ma10) ma10
    , bench "mmult hmatrix"  $ nf ((H.<>) ha10) ha10
    , bench "mmult dla"  $ nf (DLAF.multiply dla10) dla10
    ]
    ]
  cridla <- criNF 1000 (DLAF.multiply dla10) dla10
  crinh <- criNF 1000 (M.mmult ma10) ma10
  crih <- criNF 1000 ((H.<>) ha10) ha10
  crinha <- criNF 1000 (mmult va10) va10

  let cris = NumHask.Prelude.zipWith (criRun 2) ["numhask array", "numhask matrix", "hmatrix", "dla"] [crinha, crinh, crih, cridla]

  let n = 1000
  let mh x = ticks n (x H.<>) x
  let mnha x = ticks n (x `mmult`) x
  let mnh x = ticks n (x `M.mmult`) x
  let mdla x = ticks n (DLAF.multiply x) x

  rmh10 <- mh ha10
  rmnha10 <- mnha va10
  rmnh10 <- mnh ma10
  rmdla10 <- mdla dla10

  let perfs = fmap (percentile 0.5) ([fst rmnha10, fst rmnh10, fst rmh10, fst rmdla10] :: [[Cycle]])
  let perfs' = fmap (formatF 3) perfs
  let criCycles = measCycles . rescale <$> ([crinha, crinh, crih, cridla] :: [Measured])
  let ratio = fmap (formatF 3) $ zipWith (/) perfs (fromIntegral <$> criCycles)

  void $ runOutput ("bench/cri.md", LHS) ("bench/criout.md", GitHubMarkdown) $
    output "criterion"
    (Native $ (:[]) $
      table
      mempty
      ["run", "cputimes", "cycles", "nanos per cycle", "perf cycles", "perf/cri"]
      [AlignLeft, AlignRight, AlignRight, AlignRight, AlignRight]
      []
      (zipWith (\xs x -> xs <> [x]) (zipWith (\xs x -> xs <> [x]) cris perfs') ratio))

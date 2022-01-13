{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import qualified Data.Vector as V
import qualified NumHask.Array.Dynamic as D
import qualified NumHask.Array.Fixed as F
import qualified NumHask.Array.HMatrix as H
import qualified Numeric.LinearAlgebra as HMatrix
import Perf
import NumHask.Prelude as P hiding (option)
import Data.Text (unpack, Text)
import Options.Applicative
import Control.Monad

data RunType = RunMMult | RunDotSum | RunDotVector deriving (Eq, Show)

data StatType = StatAverage | StatMedian | StatBest deriving (Eq, Show)

data Options = Options
  { optionRuns :: Int,
    optionMSize :: Int,
    optionMMult :: Bool,
    optionDotSum :: Bool,
    optionDotVector :: Bool,
    optionStatType :: StatType
  } deriving (Eq, Show)

options :: Parser Options
options = Options <$>
  option auto (long "runs" <> short 'r' <> help "number of runs to perform") <*>
  option auto (long "matrix-size" <> short 'm' <> help "size of square matrix") <*>
  switch (long "include matrix-multiplication calc" <> short 'x') <*>
  switch (long "dot-sum" <> help "include dot sum (+) calc" <> short 'd') <*>
  switch (long "dot-vector" <> help "include dot vector calc" <> short 'v') <*>
  stat

opts :: ParserInfo Options
opts = info (options <**> helper)
  (fullDesc <> progDesc "benchmark testing" <> header "A performance benchmark for numhask")

stat :: Parser StatType
stat =
  flag' StatBest (long "best" <> help "report upper decile") <|>
  flag' StatMedian (long "median" <> help "report median") <|>
  pure StatAverage

tickStat :: StatType -> [Cycle] -> Text
tickStat StatBest = tenth
tickStat StatMedian = median
tickStat StatAverage = average

main :: IO ()
main = do
  o <- execParser opts
  let !n = optionRuns o
  let s = optionStatType o
  _ <- warmup 100

  putStrLn "mmult 10x10"

  when (optionMMult o) $ do
    runHMatrix s n
    runNHHMatrix s n
    runF s n
    runD s n
    when (optionDotSum o) (runFDS s n)

  when (optionDotVector o) $ do
    runDotV s n
    runDotF s n
    runDotD s n

runHMatrix :: StatType -> Int -> IO ()
runHMatrix s n = do
  -- HMatrix 10x10 matrix multiplication
  let ticksH x = ticks n (x HMatrix.<>) x
  let !h10 = (10 HMatrix.>< 10) [1 :: HMatrix.R ..]
  th10 <- ticksH h10
  putStrLn $ unpack $ "hmatrix " <> tickStat s (fst th10)

runNHHMatrix :: StatType -> Int -> IO ()
runNHHMatrix s n = do
  -- NumHask.Array.HMatrix
  let ticksH' x = ticks n (x `H.mmult`) x
  let !h'10 = [1 .. 100] :: H.Array '[10, 10] Double
  th'10 <- ticksH' h'10
  putStrLn $ unpack $ "numhask-hmatrix " <> tickStat s (fst th'10)

runF :: StatType -> Int -> IO ()
runF s n = do
  -- NumHask.Array.Fixed
  let ticksF x = ticks n (x `F.mmult`) x
  let !f10 = [1 .. 100] :: F.Array '[10, 10] Double
  tf10 <- ticksF f10
  putStrLn $ unpack $ "Fixed " <> tickStat s (fst tf10)

runFDS :: StatType -> Int -> IO ()
runFDS s n = do
  -- NumHask.Array.Fixed.dot
  let ticksF x = ticks n (F.dot sum (+) x) x
  let !f10 = [1 .. 100] :: F.Array '[10, 10] Double
  tf10 <- ticksF f10
  putStrLn $ unpack $ "Fixed-dotsum " <> tickStat s (fst tf10)

runD :: StatType -> Int -> IO ()
runD s n = do
  -- NumHask.Array.Dynamic
  let !d10 = D.fromFlatList [10, 10] [1 .. 100] :: D.Array Double
  let ticksD x = ticks n (x `D.mmult`) x
  td10 <- ticksD d10
  putStrLn $ unpack $ "Dynamic " <> tickStat s (fst td10)

runDotV :: StatType -> Int -> IO ()
runDotV s n = do
  -- Vector dot
  let !vv = V.fromList [1 .. 100] :: V.Vector Double
  let ticksDotv x = ticks n (sum . V.zipWith (*) x) x
  tvd <- ticksDotv vv
  putStrLn $ unpack $ "Vector-dot " <> tickStat s (fst tvd)

runDotF :: StatType -> Int -> IO ()
runDotF s n = do
  -- Fixed Dot
  let !vf = P.fromList [1 .. 100] :: F.Array '[100] Double
  let ticksDotF x = ticks n (F.dot sum (+) x) x
  tfd <- ticksDotF vf
  putStrLn $ unpack $ "Fixed-dot " <> tickStat s (fst tfd)

runDotD :: StatType -> Int -> IO ()
runDotD s n = do
  -- Dynamic Dot
  let !vd = D.fromFlatList [100] [1 .. 100] :: D.Array Double
  let !vd2 = D.fromFlatList [100] [101 .. 200] :: D.Array Double
  let ticksDotD x = ticks n (D.dot sum (+) vd2) x
  tdd <- ticksDotD vd
  putStrLn $ unpack $ "Dynamic-dot " <> tickStat s (fst tdd)
  pure ()

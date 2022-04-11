{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Options.Applicative
import Control.Monad
import Data.FormatN
import qualified Data.Text.IO as Text
import qualified Prelude
import Data.Text (Text)

data RunType = RunMMult | RunDotSum | RunDotVector deriving (Eq, Show)

data Options = Options
  { optionRuns :: Int,
    optionMSize :: Int,
    optionMMult :: Bool,
    optionDotSum :: Bool,
    optionDotVector :: Bool,
    optionStatDType :: StatDType
  } deriving (Eq, Show)

options :: Parser Options
options = Options <$>
  option auto (long "runs" <> short 'r' <> help "number of runs to perform") <*>
  option auto (long "matrix-size" <> short 'm' <> help "size of square matrix") <*>
  switch (long "include matrix-multiplication calc" <> short 'x') <*>
  switch (long "dot-sum" <> help "include dot sum (+) calc" <> short 'd') <*>
  switch (long "dot-vector" <> help "include dot vector calc" <> short 'v') <*>
  stat'

opts :: ParserInfo Options
opts = info (options <**> helper)
  (fullDesc <> progDesc "benchmark testing" <> header "A performance benchmark for numhask")

stat' :: Parser StatDType
stat' =
  flag' StatBest (long "best" <> help "report upper decile") <|>
  flag' StatMedian (long "median" <> help "report median") <|>
  pure StatAverage

main :: IO ()
main = do
  o <- execParser opts
  let !n = optionRuns o
  let s = optionStatDType o
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

rep :: (Prelude.Integral a) => StatDType -> Text -> [a] -> IO ()
rep s t x = Text.putStrLn $ t <> expt (Just 3) (statD s (Prelude.fromIntegral <$> x))

runHMatrix :: StatDType -> Int -> IO ()
runHMatrix s n = do
  -- HMatrix 10x10 matrix multiplication
  let ticksH x = ticks n (x HMatrix.<>) x
  let !h10 = (10 HMatrix.>< 10) [1 :: HMatrix.R ..]
  th10 <- ticksH h10
  putStrLn $ "hmatrix" <> show (statD s (Prelude.fromIntegral <$> fst th10))

runNHHMatrix :: StatDType -> Int -> IO ()
runNHHMatrix s n = do
  -- NumHask.Array.HMatrix
  let ticksH' x = ticks n (x `H.mmult`) x
  let !h'10 = [1 .. 100] :: H.Array '[10, 10] Double
  th'10 <- ticksH' h'10
  rep s "numhask-hmatrix " (fst th'10)

runF :: StatDType -> Int -> IO ()
runF s n = do
  -- NumHask.Array.Fixed
  let ticksF x = ticks n (x `F.mmult`) x
  let !f10 = [1 .. 100] :: F.Array '[10, 10] Double
  tf10 <- ticksF f10
  rep s "Fixed " (fst tf10)

runFDS :: StatDType -> Int -> IO ()
runFDS s n = do
  -- NumHask.Array.Fixed.dot
  let ticksF x = ticks n (F.dot sum (+) x) x
  let !f10 = [1 .. 100] :: F.Array '[10, 10] Double
  tf10 <- ticksF f10
  rep s "Fixed-dotsum " (fst tf10)

runD :: StatDType -> Int -> IO ()
runD s n = do
  -- NumHask.Array.Dynamic
  let !d10 = D.fromFlatList [10, 10] [1 .. 100] :: D.Array Double
  let ticksD x = ticks n (x `D.mmult`) x
  td10 <- ticksD d10
  rep s "Dynamic " (fst td10)

runDotV :: StatDType -> Int -> IO ()
runDotV s n = do
  -- Vector dot
  let !vv = V.fromList [1 .. 100] :: V.Vector Double
  let ticksDotv x = ticks n (sum . V.zipWith (*) x) x
  tvd <- ticksDotv vv
  rep s "Vector-dot " (fst tvd)

runDotF :: StatDType -> Int -> IO ()
runDotF s n = do
  -- Fixed Dot
  let !vf = P.fromList [1 .. 100] :: F.Array '[100] Double
  let ticksDotF x = ticks n (F.dot sum (+) x) x
  tfd <- ticksDotF vf
  rep s "Fixed-dot " (fst tfd)

runDotD :: StatDType -> Int -> IO ()
runDotD s n = do
  -- Dynamic Dot
  let !vd = D.fromFlatList [100] [1 .. 100] :: D.Array Double
  let !vd2 = D.fromFlatList [100] [101 .. 200] :: D.Array Double
  let ticksDotD = ticks n (D.dot sum (+) vd2)
  tdd <- ticksDotD vd
  rep s "Dynamic-dot " (fst tdd)

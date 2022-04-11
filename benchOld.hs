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
{-# LANGUAGE KindSignatures #-}

import qualified Data.Vector as V
import qualified NumHask.Array.Dynamic as D
import qualified NumHask.Array.Fixed as F
import qualified NumHask.Array.HMatrix as H
import qualified Numeric.LinearAlgebra as HMatrix
import Perf
import NumHask.Prelude as P hiding (option)
import Options.Applicative
import qualified Prelude

data MeasureType = MeasureTime | MeasureSpace | MeasureSpaceTime | MeasureAllocation deriving (Eq, Show)

data RunType = RunMMult | RunDotSum | RunDotVector deriving (Eq, Show)

data Options = Options
  { optionRuns :: Int,
    optionMSize :: Int,
    optionRunType :: RunType,
    optionMeasureType :: MeasureType,
    optionStatDType :: StatDType,
    optionsGolden :: Maybe FilePath,
    optionsRecord :: Bool,
    optionsRecordCheck :: Bool
  } deriving (Eq, Show)

parseRun :: Parser RunType
parseRun =
  flag' RunMMult (long "mmult" <> help "matrix multiplication") <|>
  flag' RunDotSum (long "sdot" <> help "dot sum (+)") <|>
  flag' RunDotVector (long "vdot" <> help "dot vector") <|>
  pure RunMMult

parseMeasure :: Parser MeasureType
parseMeasure =
  flag' MeasureTime (long "time" <> help "measure time performance") <|>
  flag' MeasureSpace (long "space" <> help "measure space performance") <|>
  flag' MeasureSpaceTime (long "spacetime" <> help "measure both space and time performance") <|>
  flag' MeasureAllocation (long "allocation" <> help "measure bytes allocated") <|>
  pure MeasureTime

options :: Parser Options
options = Options <$>
  option auto (long "runs" <> short 'r' <> help "number of runs to perform") <*>
  option auto (long "matrix-size" <> short 'm' <> help "size of square matrix") <*>
  parseRun <*>
  parseMeasure <*>
  parseStatD <*>
  optional (option str (long "golden" <> short 'g' <> help "golden file name")) <*>
  switch (long "record" <> short 'g' <> help "record the result to a golden file") <*>
  switch (long "check" <> short 'c' <> help "check versus a golden file")


opts :: ParserInfo Options
opts = info (options <**> helper)
  (fullDesc <> progDesc "benchmark testing" <> header "A performance benchmark for numhask")

main :: IO ()
main = do
  o <- execParser opts
  let !n = optionRuns o
  let s = optionStatDType o
  let r = optionRunType o
  _ <- warmup 100

  case r of
    RunMMult -> do
      runHMatrix s n
      runNHHMatrix s n
      runF s n
      runD s n
    RunDotSum -> do
      runFDS s n
    RunDotVector -> do
      runDotV s n
      runDotF s n
      runDotD s n

runHMatrix :: StatDType -> Int -> IO ()
runHMatrix s n = do
  -- HMatrix 10x10 matrix multiplication
  let ticksH x = ticks n (x HMatrix.<>) x
  let !h10 = (10 HMatrix.>< 10) [1 :: HMatrix.R ..]
  th10 <- ticksH h10
  putStrLn $ "hmatrix " <> show (statD s (Prelude.fromIntegral <$> fst th10))

runNHHMatrix :: StatDType -> Int -> IO ()
runNHHMatrix s n = do
  -- NumHask.Array.HMatrix
  let ticksH' x = ticks n (x `H.mmult`) x
  let !h'10 = [1 .. 100] :: H.Array '[10, 10] Double
  th'10 <- ticksH' h'10
  putStrLn $ "numhask-hmatrix " <> show (statD s (Prelude.fromIntegral <$> fst th'10))

runF :: StatDType -> Int -> IO ()
runF s n = do
  -- NumHask.Array.Fixed
  let ticksF x = ticks n (x `F.mmult`) x
  let !f10 = [1 .. 100] :: F.Array '[10, 10] Double
  tf10 <- ticksF f10
  putStrLn $ "Fixed " <> show (statD s (Prelude.fromIntegral <$> fst tf10))

runFDS :: StatDType -> Int -> IO ()
runFDS s n = do
  -- NumHask.Array.Fixed.dot
  let ticksF x = ticks n (F.dot sum (+) x) x
  let !f10 = [1 .. 100] :: F.Array '[10, 10] Double
  tf10 <- ticksF f10
  putStrLn $ "Fixed-dotsum " <> show (statD s (Prelude.fromIntegral <$> fst tf10))

runD :: StatDType -> Int -> IO ()
runD s n = do
  let !d10 = D.fromFlatList [10, 10] [1 .. 100] :: D.Array Double
  let ticksD x = ticks n (x `D.mmult`) x
  td10 <- ticksD d10
  putStrLn $ "Dynamic " <> show (statD s (Prelude.fromIntegral <$> fst td10))

runDotV :: StatDType -> Int -> IO ()
runDotV s n = do
  -- Vector dot
  let !vv = V.fromList [1 .. 100] :: V.Vector Double
  let ticksDotv x = ticks n (sum . V.zipWith (*) x) x
  tvd <- ticksDotv vv
  putStrLn $ "Vector-dot " <> show (statD s (Prelude.fromIntegral <$> fst tvd))

runDotF :: StatDType -> Int -> IO ()
runDotF s n = do
  -- Fixed Dot
  let !vf = P.fromList [1 .. 100] :: F.Array '[100] Double
  let ticksDotF x = ticks n (F.dot sum (+) x) x
  tfd <- ticksDotF vf
  putStrLn $ "Fixed-dot " <> show (statD s (Prelude.fromIntegral <$> fst tfd))

runDotD :: StatDType -> Int -> IO ()
runDotD s n = do
  -- Dynamic Dot
  let !vd = D.fromFlatList [100] [1 .. 100] :: D.Array Double
  let !vd2 = D.fromFlatList [100] [101 .. 200] :: D.Array Double
  let ticksDotD = ticks n (D.dot sum (+) vd2)
  tdd <- ticksDotD vd
  putStrLn $ "Dynamic-dot " <> show (statD s (Prelude.fromIntegral <$> fst tdd))
  pure ()

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
import Perf hiding (outer)
import NumHask.Prelude as P hiding (option)
import Options.Applicative
import Data.List (intercalate)
import Control.Monad

import Data.Array.ST
import Data.Array.Unboxed

data RunType = RunMMult | RunDotM | RunDotV | RunArray deriving (Eq, Show)

data Options = Options
  { optionRuns :: Int,
    optionMSize :: Int,
    optionRunType :: RunType,
    optionMeasureType :: MeasureType,
    optionStatDType :: StatDType,
    optionsGolden :: Golden,
    optionsReportConfig :: ReportConfig,
    optionRawStats :: Bool
  } deriving (Eq, Show)

parseRun :: Parser RunType
parseRun =
  flag' RunMMult (long "mmult" <> help "matrix multiplication") <|>
  flag' RunDotM (long "dotm" <> help "dot sum (+)") <|>
  flag' RunDotV (long "dotv" <> help "dot sum (+) versus vector") <|>
  flag' RunArray (long "array" <> help "array comparison") <|>
  pure RunMMult

options :: Parser Options
options = Options <$>
  option auto (value 10000 <> long "runs" <> short 'n' <> help "number of runs to perform") <*>
  option auto (value 10 <> long "matrix-size" <> short 'm' <> help "size of square matrix") <*>
  parseRun <*>
  parseMeasure <*>
  parseStatD <*>
  parseGolden "golden" <*>
  parseReportConfig defaultReportConfig <*>
  switch (long "raw" <> short 'w' <> help "write raw statistics to file")

opts :: ParserInfo Options
opts = info (options <**> helper)
  (fullDesc <> progDesc "benchmark testing" <> header "A performance benchmark for numhask")

main :: IO ()
main = do
  o <- execParser opts
  let !n = optionRuns o
  let s = optionStatDType o
  let r = optionRunType o
  let m = optionMSize o
  let mt = optionMeasureType o
  let gold' = optionsGolden o
  let gold =
        case golden gold' of
          "other/golden.csv" ->
            gold'
            { golden = "other/" <>
              intercalate "-" [show r, show n, show m, show s, show mt] <>
              ".csv" }
          _ -> gold'
  let cfg = optionsReportConfig o
  let w = optionRawStats o
  let raw = "other/" <>
              intercalate "-" [show r, show n, show m, show s, show mt] <>
              ".map"
  _ <- warmup 100
  let go = run_ w raw cfg gold mt s n
  case r of
    RunMMult -> do
      go (runMM 10)
    RunDotM -> do
      go (runDotM 10)
    RunDotV -> do
      go (runDotV 100)
    RunArray -> do
      go (runArray 10)

run_ :: Bool -> FilePath -> ReportConfig -> Golden -> MeasureType -> StatDType -> Int -> PerfT IO [[Double]] a -> IO ()
run_ w raw' cfg gold mt s n f = do
      m <- execPerfT (measureDs mt n) f
      when w (writeFile raw' (show m))
      report cfg gold (measureLabels mt) (statify s m)
{-# Inline run_ #-}

runMM :: (Semigroup t) => Int -> PerfT IO t ()
runMM m = do
  _ <- fap "numhask-hmatrix" (\x -> x `H.mmult` x) ([1 .. (m'*m')] :: H.Array '[10, 10] Double)
  _ <- fap "fixed" (\x -> x `F.mmult` x) ([1 .. (m'*m')] :: F.Array '[10, 10] Double)
  _ <- fap "dynamic" (\x -> x `D.mmult` x) (D.fromFlatList [m, m] [1 .. (m'*m')] :: D.Array Double)
  _ <- fap "hmatrix" (\x -> x HMatrix.<> x) ((m HMatrix.>< m) [1 :: HMatrix.R ..])
  pure ()
  where
    m' = fromIntegral m
{-# Inline runMM #-}

runDotM :: (Semigroup t) => Int -> PerfT IO t ()
runDotM m = do
  _ <- fap "fixed-mmult" (\x -> x `F.mmult` x) ([1 .. (m'*m')] :: F.Array '[10, 10] Double)
  _ <- fap "dynamic-mmult" (\x -> x `D.mmult` x) (D.fromFlatList [m, m] [1 .. (m'*m')] :: D.Array Double)
  _ <- fap "fixed-dot" (\x -> F.dot sum (+) x x) ([1 .. (m'*m')] :: F.Array '[10, 10] Double)
  _ <- fap "dynamic-dot" (\x -> D.dot sum (+) x x) (D.fromFlatList [m, m] [1 .. (m'*m')] :: D.Array Double)
  pure ()
  where
    m' = fromIntegral m
{-# Inline runDotM #-}

runDotV :: (Semigroup t) => Int -> PerfT IO t ()
runDotV m = do
  _ <- fap "vector-dot" (sum . V.zipWith (*) v) v
  _ <- fap "fixed-dot" (F.dot sum (+) f) f
  pure ()
  where
    m' = fromIntegral m
    !v = V.fromList [1 .. m'] :: V.Vector Double
    !f = [1 .. m'] :: F.Array '[100] Double
{-# Inline runDotV #-}

runArray :: (Semigroup t) => Int -> PerfT IO t ()
runArray m = do
  _ <- fap "fixed-dot" (\x -> F.dot sum (+) x x) ([1 .. (m'*m')] :: F.Array '[10, 10] Double)
  _ <- fap "array-mmult" (\x -> matProd x 10 10 x 10 10) ma
  pure ()
  where
    m' = fromIntegral m
    !ma = mkMat 10 10 1 1
{-# Inline runArray #-}

-- | example of Array taken from https://stackoverflow.com/questions/11768656/reasonably-efficient-pure-functional-matrix-product-in-haskell
matProd :: UArray Int Float -> Int -> Int -> UArray Int Float -> Int -> Int -> UArray Int Float
matProd a ra ca b rb cb =
    let (al,ah)     = bounds a
        (bl,bh)     = bounds b
        {-# INLINE getA #-}
        getA i j    = a!(i*ca + j)
        {-# INLINE getB #-}
        getB i j    = b!(i*cb + j)
        {-# INLINE idx #-}
        idx i j     = i*cb + j
    in if al /= 0 || ah+1 /= ra*ca || bl /= 0 || bh+1 /= rb*cb || ca /= rb
         then error $ "Matrices not fitting: " ++ show (ra,ca,al,ah,rb,cb,bl,bh)
         else runSTUArray $ do
            arr <- newArray (0,ra*cb-1) 0
            let outer i j
                    | ra <= i   = return arr
                    | cb <= j   = outer (i+1) 0
                    | otherwise = do
                        !x <- inner i j 0 0
                        writeArray arr (idx i j) x
                        outer i (j+1)
                inner i j k !y
                    | ca <= k   = return y
                    | otherwise = inner i j (k+1) (y + getA i k * getB k j)
            outer 0 0
{-# Inline matProd #-}

mkMat :: Int -> Int -> Float -> Float -> UArray Int Float
mkMat rs cs x d = runSTUArray $ do
    let !r = rs - 1
        !c = cs - 1
        {-# INLINE idx #-}
        idx i j = cs*i + j
    arr <- newArray (0,rs*cs-1) 0
    let outer i j y
            | r < i     = return arr
            | c < j     = outer (i+1) 0 y
            | otherwise = do
                writeArray arr (idx i j) y
                outer i (j+1) (y + d)
    outer 0 0 x

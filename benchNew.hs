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
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.FormatN
import GHC.TypeNats

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

rioOrg :: Maybe FilePath -> Maybe FilePath -> StatDType -> [Text] -> Map.Map [Text] [[Double]] -> IO ()
rioOrg check record s labels m = do
    case check of
      Nothing -> printOrg (expt (Just 3) <$> m')
      Just fp -> degradePrint defaultDegradeConfig fp m'
    mapM_ (`writeResult` m') record
    where
      m' = Map.fromList $ mconcat $ (\(ks,xss) -> zipWith (\x l -> (ks <> [l], statD s x)) (List.transpose xss) labels) <$> Map.toList m

-- * org-mode formatting
outercalate :: Text -> [Text] -> Text
outercalate c ts = c <> Text.intercalate c ts <> c

printOrgHeader :: Map.Map [Text] a -> [Text] -> IO ()
printOrgHeader m ts = do
  Text.putStrLn $ outercalate "|" ((("label" <>) . Text.pack . show <$> [1..labelCols]) <> ts)
  Text.putStrLn $ outercalate "|" (replicate (labelCols+1) "---")
  pure ()
    where
      labelCols = maximum $ length <$> Map.keys m

printOrg :: Map.Map [Text] Text -> IO ()
printOrg m = do
  printOrgHeader m ["results"]
  _ <- Map.traverseWithKey (\k a -> Text.putStrLn (outercalate "|" (k <> [a]))) m
  pure ()

main :: IO ()
main = do
  o <- execParser opts
  let !n = optionRuns o
  let s = optionStatDType o
  let r = optionRunType o
  let mt = optionMeasureType o
  let fp = fromMaybe ("other/" <> show r <> ".csv") (optionsGolden o)
  let record = bool Nothing (Just fp) (optionsRecord o)
  let check = bool Nothing (Just fp) (optionsRecordCheck o)
  let rio = rioOrg check record s
  _ <- warmup 100

  case r of
    RunMMult -> do
      run_ rio mt n runMM
    RunDotSum -> do
      run_ rio mt n runFDS_
    RunDotVector -> do
      run_ rio mt n runDotV_
      run_ rio mt n runDotF_
      run_ rio mt n runDotD_

-- | unification of the different measurements to being a list of doubles.
measureDs :: MeasureType -> Int -> Measure IO [[Double]]
measureDs mt n =
  case mt of
    MeasureTime -> fmap ((:[]) . Prelude.fromIntegral) <$> times n
    MeasureSpace -> toMeasureN n (ssToList <$> space False)
    MeasureSpaceTime -> toMeasureN n ((\x y -> ssToList x <> [Prelude.fromIntegral y]) <$> space False <*> stepTime)
    MeasureAllocation -> fmap ((:[]) . Prelude.fromIntegral) <$> toMeasureN n (allocation False)

-- | unification of the different measurements to being a list of doubles.
measureLabels :: MeasureType -> [Text]
measureLabels mt =
  case mt of
    MeasureTime -> ["time"]
    MeasureSpace -> spaceLabels
    MeasureSpaceTime -> spaceLabels <> ["time"]
    MeasureAllocation -> ["allocation"]

run_ :: ([Text] -> Map.Map [Text] [[Double]] -> IO b) -> MeasureType -> Int -> PerfT IO [[Double]] a -> IO b
run_ rio mt n f = do
      m <- execPerfT (measureDs mt n) f
      rio (measureLabels mt) (Map.mapKeys (:[]) m)
{-# Inline run_ #-}

runMM :: (Semigroup t) => PerfT IO t ()
runMM = do
  _ <- fap "hmatrix" (\x -> x HMatrix.<> x) ((10 HMatrix.>< 10) [1 :: HMatrix.R ..])
  _ <- fap "numhask-hmatrix" (\x -> x `H.mmult` x) ([1 .. 100] :: H.Array '[10, 10] Double)
  _ <- fap "fixed" (\x -> x `F.mmult` x) ([1 .. 100] :: F.Array '[10, 10] Double)
  _ <- fap "dynamic" (\x -> x `D.mmult` x) (D.fromFlatList [10, 10] [1 .. 100] :: D.Array Double)
  pure ()
{-# Inline runMM #-}

runFDS_ :: (Semigroup t) => PerfT IO t (F.Array '[10,10] Double)
runFDS_ = fap "fixed" (F.dot sum (+) x) x
  where
    x = [1 .. 100] :: F.Array '[10, 10] Double

runDotV_ :: (Semigroup t) => PerfT IO t Double
runDotV_ = fap "dynamic" (sum . V.zipWith (*) x) x
  where
    x = V.fromList [1 .. 100] :: V.Vector Double

runDotF_ :: (Semigroup t) => PerfT IO t (F.Array ('[]::[Nat]) Double)
runDotF_ = fap "dynamic" (F.dot sum (+) x) x
  where
    x = P.fromList [1 .. 100] :: F.Array '[100] Double

runDotD_ :: (Semigroup t) => PerfT IO t (D.Array Double)
runDotD_ = fap "dynamic" (D.dot sum (+) vd) vd2
  where
    vd = D.fromFlatList [100] [1 .. 100] :: D.Array Double
    vd2 = D.fromFlatList [100] [101 .. 200] :: D.Array Double

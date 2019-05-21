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
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import qualified Protolude as P
import NumHask.Array
import NumHask.Prelude
import Options.Generic
import Perf
import Perf.Analysis
import qualified Numeric.LinearAlgebra as H
import qualified Data.Vector as V
import qualified Statistics.Matrix as DLA
import System.Random.MWC

instance NFData DLA.Matrix where
  rnf m = seq m ()

oneRunList10 g f = do
  ra <- sequence $ replicate 100 (uniform g :: IO Double)
  rb <- sequence $ replicate 100 (uniform g :: IO Double)
  let !aa = fromList ra :: Array [] '[10, 10] Double
  let ab = fromList rb :: Array [] '[10, 10] Double
  (res, r) <- tickNoinline (f aa) ab
  pure (res, r)

oneRunVector10 g f = do
  ra <- sequence $ replicate 100 (uniform g :: IO Double)
  rb <- sequence $ replicate 100 (uniform g :: IO Double)
  let !aa = fromList ra :: Array V.Vector '[10, 10] Double
  let ab = fromList rb :: Array V.Vector '[10, 10] Double
  (res, r) <- tickNoinline (f aa) ab
  pure (res, r)

oneRunList100 g f = do
  ra <- sequence $ replicate 10000 (uniform g :: IO Double)
  rb <- sequence $ replicate 10000 (uniform g :: IO Double)
  let !aa = fromList ra :: Array [] '[100, 100] Double
  let ab = fromList rb :: Array [] '[100, 100] Double
  (res, r) <- tickNoinline (f aa) ab
  pure (res, r)

oneRunVector100 g f = do
  ra <- sequence $ replicate 10000 (uniform g :: IO Double)
  rb <- sequence $ replicate 10000 (uniform g :: IO Double)
  let !aa = fromList ra :: Array V.Vector '[100, 100] Double
  let ab = fromList rb :: Array V.Vector '[100, 100] Double
  (res, r) <- tickNoinline (f aa) ab
  pure (res, r)

oneRunHMatrix g f sz = do
  ra <- sequence $ replicate (sz*sz) (uniform g :: IO Double)
  rb <- sequence $ replicate (sz*sz) (uniform g :: IO Double)
  let !aa = H.matrix sz ra
  let !ab = H.matrix sz rb
  (res, r) <- tickNoinline (f aa) ab
  pure (res, r)

oneRunDLA g f sz = do
  ra <- sequence $ replicate (sz*sz) (uniform g :: IO Double)
  rb <- sequence $ replicate (sz*sz) (uniform g :: IO Double)
  let !aa = DLA.fromList sz sz ra
  let !ab = DLA.fromList sz sz rb
  (res, r) <- tickNoinline (f aa) ab
  pure (res, r)

oneRunListV10 g f = do
  ra <- sequence $ replicate 10 (uniform g :: IO Double)
  rb <- sequence $ replicate 10 (uniform g :: IO Double)
  let !aa = fromList ra :: Array [] '[10] Double
  let !ab = fromList rb :: Array [] '[10] Double
  (res, r) <- tickNoinline (f aa) ab
  pure (res, r)

oneRunVectorV10 g f = do
  ra <- sequence $ replicate 10 (uniform g :: IO Double)
  rb <- sequence $ replicate 10 (uniform g :: IO Double)
  let !aa = fromList ra :: Array V.Vector '[10] Double
  let !ab = fromList rb :: Array V.Vector '[10] Double
  (res, r) <- tickNoinline (f aa) ab
  pure (res, r)

oneRunHMatrixV g f sz = do
  ra <- sequence $ replicate sz (uniform g :: IO Double)
  rb <- sequence $ replicate sz (uniform g :: IO Double)
  let !aa = H.vector ra
  let !ab = H.vector rb
  (res, r) <- tickNoinline (f aa) ab
  pure (res, r)

oneRunDLAV g f sz = do
  ra <- sequence $ replicate sz (uniform g :: IO Double)
  rb <- sequence $ replicate sz (uniform g :: IO Double)
  let !aa = DLA.fromList 1 sz ra
  let !ab = DLA.fromList sz 1 rb
  (res, r) <- tickNoinline (f aa) ab
  pure (res, r)


data Opts = Opts
  { runs :: Maybe Int -- <?> "number of runs"
  -- , size :: Maybe Int -- <?> "size of matrix"
  } deriving (Generic, Show)

instance ParseRecord Opts

main :: IO ()
main = do
  o :: Opts <- getRecord "benchmarking numhask array"
  let !n = fromMaybe 100 (runs o)
  _ <- warmup 100
  g <- create

  -- sz = 10 run
  let sz = 10 :: Int

  -- there is no way to avoid the hardcode :(
  let !aa = [1 ..] :: Array [] '[10, 10] Double
  let !ab = [0 ..] :: Array [] '[10, 10] Double

  let !va = [1 ..] :: Array V.Vector '[10, 10] Double
  let !vb = [0 ..] :: Array V.Vector '[10, 10] Double

  let !ha = (sz H.>< sz) [1 :: H.Z ..]
  let !hb = (sz H.>< sz) [1 :: H.Z ..]

  let !dlaa = DLA.fromList sz sz [1 .. (fromIntegral $ sz*sz)]
  let !dlab = DLA.fromList sz sz [0 .. (fromIntegral $ sz*sz - 1)]

  (rmmult, _) <- ticks n (NumHask.Array.mmult aa) ab
  (rmmulth, _) <- ticks n (ha H.<>) hb
  (rmmultv, _) <- ticks n (NumHask.Array.mmult va) vb
  (rmmultdla, _) <- ticks n (DLA.multiply dlaa) dlab

  writeFile "other/array.md" $ code
    [ "square matrix size: " <> show sz
    , ""
    , "mmult"
    , formatRunHeader
    , ""
    , formatRun "numhask []" 2 rmmult
    , formatRun "numhask Boxed" 2 rmmultv
    , formatRun "hmatrix" 2 rmmulth
    , formatRun "DLA" 2 rmmultdla
    ]

  xList <- sequence $ replicate n (oneRunList10 g mmult)
  print $ sum $ sum $ fmap snd xList
  xVec <- sequence $ replicate n (oneRunVector10 g mmult)
  print $ sum $ sum $ fmap snd xVec
  xH <- sequence $ replicate n (oneRunHMatrix g (H.<>) sz)
  print $ P.sum $ fmap P.sum $ fmap (fmap P.sum) $ fmap H.toLists $ fmap snd xH
  xDLA <- sequence $ replicate n (oneRunDLA g DLA.multiply sz)
  print $ sum $ fmap sum $ fmap (fmap sum) $ fmap DLA.toRowLists $ fmap snd xDLA

  appendFile "other/array.md" $ code
    [ "random version square matrix size: " <> show sz
    , ""
    , "mmult, randoms"
    , formatRunHeader
    , ""
    , formatRun "numhask []" 2 (fmap fst xList)
    , formatRun "numhask Vector" 2 (fmap fst xVec)
    , formatRun "HMatrix" 2 (fmap fst xH)
    , formatRun "DLA" 2 (fmap fst xDLA)
    ]

  xList <- sequence $ replicate n (oneRunListV10 g (<.>))
  print $ sum $ fmap snd xList
  xVec <- sequence $ replicate n (oneRunVectorV10 g (<.>))
  print $ sum $ fmap snd xVec
  xH <- sequence $ replicate n (oneRunHMatrixV g (H.<.>) sz)
  print $ P.sum $ fmap snd xH
  xDLA <- sequence $ replicate n (oneRunDLAV g DLA.multiply sz)
  print $ sum $ fmap sum $ fmap (fmap sum) $ fmap DLA.toRowLists $ fmap snd xDLA

  appendFile "other/array.md" $ code
    [ "vector inner product size: " <> show sz
    , ""
    , "<.>, randoms"
    , formatRunHeader
    , ""
    , formatRun "numhask []" 2 (fmap fst xList)
    , formatRun "numhask Vector" 2 (fmap fst xVec)
    , formatRun "HMatrix" 2 (fmap fst xH)
    , formatRun "DLA (row by column)" 2 (fmap fst xDLA)
    ]



  -- numhask operations
  (rrow, _) <- ticks n (NumHask.Array.row (Proxy :: Proxy 4)) ab
  (rcol, _) <- ticks n (NumHask.Array.col (Proxy :: Proxy 4)) ab
  (runsaferow, _) <- ticks n (NumHask.Array.unsafeRow 0) ab
  (runsafecol, _) <- ticks n (NumHask.Array.unsafeCol 0) ab
  (runsafeindex, _) <- ticks n (NumHask.Array.unsafeIndex ab) [2, 3]
  (rconcat, _) <- ticks n (concatenate (Proxy :: Proxy 2) aa) aa
  (rtranspose, _) <- ticks n NumHask.Array.transpose aa

  writeFile "other/ops.md" $ code
    [ "square matrix size: " <> show sz
    , formatRunHeader
    , ""
    , formatRun "row" 2 rrow
    , formatRun "col" 2 rcol
    , formatRun "unsafeRow" 2 runsaferow
    , formatRun "unsafeCol" 2 runsafecol
    , formatRun "unsafeIndex" 2 runsafeindex
    , formatRun "concat" 2 rconcat
    , formatRun "transpose" 2 rtranspose
    ]

  pure ()

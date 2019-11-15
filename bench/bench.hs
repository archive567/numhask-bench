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
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import qualified Data.Text as Text
import qualified Data.Vector as V
import NumHask.Array.Dynamic as NAD
import NumHask.Array.Fixed as NAF
import qualified NumHask.Array.HMatrix as NAH
import NumHask.Prelude as NH
import qualified Numeric.LinearAlgebra as H
import Options.Generic
import Perf
import Perf.Analysis
import qualified Protolude as P
import Readme.Lhs
import qualified Statistics.Matrix as DLA
import qualified Statistics.Matrix.Fast as DLAF

-- | format a run median
formatMedian :: (P.Integral a) => Text -> [[a]] -> [Text]
formatMedian label xss =
  [label]
    <> (formatF 2 . percentile 0.5 <$> xss)

formatRunsMedian :: (P.Integral a) => [Text] -> [(Text, [[a]])] -> Block
formatRunsMedian h rs =
  table
    mempty
    (["run"] <> h)
    ([AlignLeft] <> replicate n AlignRight)
    []
    (fmap (uncurry formatMedian) rs)
  where
    n = length h

newtype Opts
  = Opts
      { runs :: Maybe Int -- <?> "number of runs"
      }
  deriving (Generic, Show)

instance ParseRecord Opts

main :: IO ()
main = do
  o :: Opts <- getRecord "benchmarking numhask array"
  let !n = fromMaybe 100 (runs o)
  _ <- warmup 100
  let ts :: [Text]
      ts = ["NAF", "NAD", "NAH", "V", "H"]

  let ma x = ticks n (x `NAF.mmult`) x
  let mnad x = ticks n (x `NAD.mmult`) x
  let mnah x = ticks n (x `NAH.mmult`) x
  let mh x = ticks n (x H.<>) x
  let md x = ticks n (DLAF.multiply x) x
  let !ma10 = [1 .. 100] :: NAF.Array '[10, 10] Double
  let !mnah10 = [1 .. 100] :: NAH.HArray '[10, 10] Double
  let !mnad10 = fromFlatList [10, 10] [1 .. 100] :: NAD.DArray Double
  let !mh10 = (10 H.>< 10) [1 :: H.R ..]
  let !md10 = DLA.fromList 10 10 [1 .. (fromIntegral $ (10 :: Int) * 10)]
  rma10 <- ma ma10
  rmh10 <- mh mh10
  rmd10 <- md md10
  rmnah10 <- mnah mnah10
  rmnad10 <- mnad mnad10
  putStrLn ("mmult 10x10" :: Text)
  putStrLn $ ("hmatrix " :: Text) <> formatF 2 (percentile 0.5 (fst rmh10))
  putStrLn $ ("NAF " :: Text) <> formatF 2 (percentile 0.5 (fst rma10))
  putStrLn $ ("Statistics.Matrix.Fast " :: Text) <> formatF 2 (percentile 0.5 (fst rmd10))
  putStrLn $ ("NAH " :: Text) <> formatF 2 (percentile 0.5 (fst rmnah10))
  putStrLn $ ("NAD " :: Text) <> formatF 2 (percentile 0.5 (fst rmnad10))

  r1000 <- dot100 100
  r100 <- dot100 100
  r10 <- dot10 100
  r2 <- dot2 100

  void
    $ runOutput
      ("other/results_.md", GitHubMarkdown)
      ("results.md", GitHubMarkdown)
    $ do
      output "values" $ Native $ (: []) $
        bool
          (table mempty [] [] [] [[Text.pack . show <$> fst $ r100]])
          (Para [Str "results agree"])
          (and $ (==) <$> fst r100 <*> fst r100)
      output "inner" $
        Native
          [ formatRunsMedian
              ["2", "10", "100", "1000"]
              (zip ts $ P.transpose [snd r2, snd r10, snd r100, snd r1000])
          ]
      output "mmult" $
        Native
          [ formatRunsMedian
              ["10"]
              [ ("NumHask.Array.Fixed", [fst rma10]),
                ("Numeric.LinearAlgebra.R", [fst rmh10]),
                ("Statistics.Matrix.Fast", [fst rmd10])
              ]
          ]

tdotnaf n x = ticks n (dot sum (*) x) x
tdotnad n x = ticks n (NAD.dot' x) x
tdotnah n x = ticks n (x NH.<.>) x
tdotv n x = ticks n (sum . V.zipWith (*) x) x
tdoth n x = ticks n (x H.<.>) x

dot1000 :: Int -> IO ([Double], [[Cycle]])
dot1000 n = do
  let !vnaf = fromList [1 .. 1000] :: NAF.Array '[1000] Double
  let !vnad = fromFlatList [1000] [1 .. 1000] :: NAD.DArray Double
  let !vnah = fromList [1 .. 1000] :: NAH.HArray '[1000] Double
  let !vh = H.fromList [1 :: H.R .. 1000]
  let !vv = V.fromList [1 .. 1000] :: V.Vector Double
  (pnaf, rnaf) <- tdotnaf n vnaf
  (pnad, rnad) <- tdotnad n vnad
  (pnah, rnah) <- tdotnah n vnah
  (pv, rv) <- tdotv n vv
  (ph, rh) <- tdoth n vh
  let vs :: [Double]
      vs = [NAF.fromScalar rnaf, rnad, rnah, rv, rh]
  pure (vs, [pnaf,pnad,pnah,pv,ph])

dot100 :: Int -> IO ([Double], [[Cycle]])
dot100 n = do
  let !vnaf = fromList [1 .. 100] :: NAF.Array '[100] Double
  let !vnad = fromFlatList [100] [1 .. 100] :: NAD.DArray Double
  let !vnah = fromList [1 .. 100] :: NAH.HArray '[100] Double
  let !vh = H.fromList [1 :: H.R .. 100]
  let !vv = V.fromList [1 .. 100] :: V.Vector Double
  (pnaf, rnaf) <- tdotnaf n vnaf
  (pnad, rnad) <- tdotnad n vnad
  (pnah, rnah) <- tdotnah n vnah
  (pv, rv) <- tdotv n vv
  (ph, rh) <- tdoth n vh
  let vs :: [Double]
      vs = [NAF.fromScalar rnaf, rnad, rnah, rv, rh]
  pure (vs, [pnaf,pnad,pnah,pv,ph])

dot10 :: Int -> IO ([Double], [[Cycle]])
dot10 n = do
  let !vnaf = fromList [1 .. 10] :: NAF.Array '[10] Double
  let !vnad = fromFlatList [10] [1 .. 10] :: NAD.DArray Double
  let !vnah = fromList [1 .. 10] :: NAH.HArray '[10] Double
  let !vh = H.fromList [1 :: H.R .. 10]
  let !vv = V.fromList [1 .. 10] :: V.Vector Double
  (pnaf, rnaf) <- tdotnaf n vnaf
  (pnad, rnad) <- tdotnad n vnad
  (pnah, rnah) <- tdotnah n vnah
  (pv, rv) <- tdotv n vv
  (ph, rh) <- tdoth n vh
  let vs :: [Double]
      vs = [NAF.fromScalar rnaf, rnad, rnah, rv, rh]
  pure (vs, [pnaf,pnad,pnah,pv,ph])

dot2 :: Int -> IO ([Double], [[Cycle]])
dot2 n = do
  let !vnaf = fromList [1 .. 2] :: NAF.Array '[2] Double
  let !vnad = fromFlatList [2] [1 .. 2] :: NAD.DArray Double
  let !vnah = fromList [1 .. 2] :: NAH.HArray '[2] Double
  let !vh = H.fromList [1 :: H.R .. 2]
  let !vv = V.fromList [1 .. 2] :: V.Vector Double
  (pnaf, rnaf) <- tdotnaf n vnaf
  (pnad, rnad) <- tdotnad n vnad
  (pnah, rnah) <- tdotnah n vnah
  (pv, rv) <- tdotv n vv
  (ph, rh) <- tdoth n vh
  let vs :: [Double]
      vs = [NAF.fromScalar rnaf, rnad, rnah, rv, rh]
  pure (vs, [pnaf,pnad,pnah,pv,ph])


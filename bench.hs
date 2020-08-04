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
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified NumHask.Array.Dynamic as D
import qualified NumHask.Array.Fixed as F
import qualified NumHask.Array.HMatrix as H
import qualified NumHask.Array.Massiv as M
import NumHask.Prelude as NH
import qualified Numeric.LinearAlgebra as HMatrix
import Options.Generic
import Perf
import Readme.Lhs
import Readme.Format
import NumHask.Prelude as P
import qualified Text.Pandoc.Builder as Pandoc

newtype Opts
  = Opts
      { runs :: Maybe Int -- <?> "number of runs"
      }
  deriving (Generic, Show)

instance ParseRecord Opts

main :: IO ()
main = do
  let !n = 100
  _ <- warmup 100
  let ma x = ticks n (x `F.mmult`) x
  let mam x = ticks n (M.dot sum (*) x) x
  let mnad x = ticks n (x `D.mmult`) x
  let mnah x = ticks n (x `H.mmult`) x
  let mh x = ticks n (x HMatrix.<>) x
  let !ma10 = [1 .. 100] :: F.Array '[10, 10] Double
  let !mam10 = [1 .. 100] :: M.Array '[10, 10] Double
  let !mnah10 = [1 .. 100] :: H.Array '[10, 10] Double
  let !mnah10 = [1 .. 100] :: H.Array '[10, 10] Double
  let !mnad10 = D.fromFlatList [10, 10] [1 .. 100] :: D.Array Double
  let !mh10 = (10 HMatrix.>< 10) [1 :: HMatrix.R ..]
  rma10 <- ma ma10
  mma10 <- mam mam10
  drma10 <- mnad (F.toDynamic ma10)
  hrma10 <- mnah mnah10
  rmh10 <- mh mh10
  rmnah10 <- mnah mnah10
  rmnad10 <- mnad mnad10
  putStrLn ("mmult 10x10" :: Text)
  putStrLn $ ("hmatrix " :: Text) <> fixed 2 (percentile 0.5 (fromIntegral <$> fst rmh10))
  putStrLn $ ("Fixed " :: Text) <> fixed 2 (percentile 0.5 (fromIntegral <$> fst rma10))
  putStrLn $ ("Massive " :: Text) <> fixed 2 (percentile 0.5 (fromIntegral <$> fst mma10))
  putStrLn $ ("HMatrix " :: Text) <> fixed 2 (percentile 0.5 (fromIntegral <$> fst rmnah10))
  putStrLn $ ("Dynamic " :: Text) <> fixed 2 (percentile 0.5 (fromIntegral <$> fst rmnad10))

  r2 <- dot100 2
  r100 <- dot100 100

  pure ()


tdotnaf n x = ticks n (F.dot sum (+) x) x
tdotnad n x = ticks n (D.dot sum (+) x) x
tdotv n x = ticks n (sum . V.zipWith (*) x) x

dot100 :: Int -> IO ([Double], [[Cycle]])
dot100 n = do
  let !vnaf = P.fromList [1 .. 100] :: F.Array '[100] Double
  let !vnad = D.fromFlatList [100] [1 .. 100] :: D.Array Double
  let !vv = V.fromList [1 .. 100] :: V.Vector Double
  (pnaf, rnaf) <- tdotnaf n vnaf
  (pnad, _) <- tdotnad n vnad
  (pv, rv) <- tdotv n vv
  let vs :: [Double]
      vs = [F.fromScalar rnaf, rv]
  pure (vs, [pnaf,pnad,pv])


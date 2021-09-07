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

import qualified Data.Vector as V
import qualified NumHask.Array.Dynamic as D
import qualified NumHask.Array.Fixed as F
import qualified NumHask.Array.HMatrix as H
import NumHask.Prelude as NH
import NumHask.Space (quantile)
import qualified Numeric.LinearAlgebra as HMatrix
import Perf
import Data.FormatN
import NumHask.Prelude as P
import qualified Prelude
import Data.Text (Text, unpack)

main :: IO ()
main = do
  let !n = 100
  _ <- warmup 100
  let ma x = ticks n (x `F.mmult`) x
  let mnad x = ticks n (x `D.mmult`) x
  let mnah x = ticks n (x `H.mmult`) x
  let mh x = ticks n (x HMatrix.<>) x
  let !ma10 = [1 .. 100] :: F.Array '[10, 10] Double
  let !mnah10 = [1 .. 100] :: H.Array '[10, 10] Double
  let !mnah10 = [1 .. 100] :: H.Array '[10, 10] Double
  let !mnad10 = D.fromFlatList [10, 10] [1 .. 100] :: D.Array Double
  let !mh10 = (10 HMatrix.>< 10) [1 :: HMatrix.R ..]
  rma10 <- ma ma10
  -- FIXME: NFData instance needed
  -- mma10 <- mam mam10
  drma10 <- mnad (F.toDynamic ma10)
  hrma10 <- mnah mnah10
  rmh10 <- mh mh10
  rmnah10 <- mnah mnah10
  rmnad10 <- mnad mnad10
  putStrLn ("mmult 10x10")
  putStrLn $ unpack $ ("hmatrix ") <> fixed 2 (quantile 0.5 (Prelude.fromIntegral <$> fst rmh10))
  putStrLn $ unpack $ ("Fixed ") <> fixed 2 (quantile 0.5 (Prelude.fromIntegral <$> fst rma10))
  putStrLn $ unpack $ ("HMatrix " :: Text) <> fixed 2 (quantile 0.5 (Prelude.fromIntegral <$> fst rmnah10))
  putStrLn $ unpack $ ("Dynamic " :: Text) <> fixed 2 (quantile 0.5 (Prelude.fromIntegral <$> fst rmnad10))

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


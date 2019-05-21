{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
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

import NumHask.Array
import NumHask.Prelude
import Perf
import Perf.Analysis
import System.Random.MWC
import qualified Data.Vector as V

oneRunList g = do
  ra <- sequence $ replicate 100 (uniform g :: IO Double)
  rb <- sequence $ replicate 100 (uniform g :: IO Double)
  let !aa = fromList ra :: Array [] '[10, 10] Double
  let ab = fromList rb :: Array [] '[10, 10] Double
  (res, r) <- tickNoinline (NumHask.Array.mmult aa) ab
  pure (res, r)

oneRunVector g = do
  ra <- sequence $ replicate 100 (uniform g :: IO Double)
  rb <- sequence $ replicate 100 (uniform g :: IO Double)
  let !aa = fromList ra :: Array V.Vector '[10, 10] Double
  let ab = fromList rb :: Array V.Vector '[10, 10] Double
  (res, r) <- tickNoinline (NumHask.Array.mmult aa) ab
  pure (res, r)

main :: IO ()
main = do
  g <- create
  _ <- warmup 100
  -- sz = 10 run
  x <- sequence $ replicate 1000 (oneRunList g)
  print $ sum $ sum $ fmap snd x
  putStrLn $ formatRun "numhask []" 2 (fmap fst x)
  print $ percentile 0.1 (fmap fst x)
  x <- sequence $ replicate 1000 (oneRunVector g)
  print $ sum $ sum $ fmap snd x
  putStrLn $ formatRun "numhask Vector" 2 (fmap fst x)
  -- tickNoinline (\x -> sequence $ replicate x (oneRunList g)) 1000
  print $ sum $ sum $ fmap snd x
  putStrLn $ formatRun "numhask []" 2 (fmap fst x)
  print $ percentile 0.1 (fmap fst x)
   



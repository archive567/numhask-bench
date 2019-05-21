{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import NumHask.Array
import NumHask.Prelude
import System.Random.MWC
import Criterion.Main


main :: IO ()
main = do
  g <- create
  ra <- sequence $ replicate 100 (uniform g :: IO Double)
  rb <- sequence $ replicate 100 (uniform g :: IO Double)
  let aa = fromList ra :: Array [] '[10, 10] Double
  let ab = fromList rb :: Array [] '[10, 10] Double
  defaultMain [
    bgroup "cri"
    [ bench "nf numhask [] mmult"  $ nf (mmult aa) ab
    , bench "nf numhask [] (NumHask.Array.><)"  $ nf ((NumHask.Array.><) aa) ab
    , bench "nf numhask [] (NumHask.Prelude.><)"  $ nf ((NumHask.Prelude.><) aa) ab
    , bench "nf numhask [] sum . mmult"  $ nf (sum . mmult aa) ab
    , bench "nf numhask [] sum"  $ nf sum (mmult aa ab)
    ]
    ]


Current Development
---

Vector Inner Product

```{.output .inner}
```

Matrix Multiplication
```{.output .mmult}
```

code
===

\begin{code}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- import qualified Protolude as P
-- import NumHask.Array.Simple as A
import NumHask.Vector as VNH
import NumHask.Prelude as NH
-- import Options.Generic
import Perf
-- import Perf.Analysis
-- import qualified Numeric.LinearAlgebra as H
-- import qualified Data.Vector as V
-- import Readme.Lhs
-- import qualified Statistics.Matrix.Fast as DLAF
-- import qualified Statistics.Matrix as DLA

main :: IO ()
main = do
  let n = 1000
  -- let tdotv x = ticks n (sum . V.zipWith (*) x) x
  let tdota x = ticks n (x NH.<.>) x
  -- let tdoth x = ticks n (x H.<.>) x

  -- size = 100
  -- let !vv100 = V.fromList [1..100] :: V.Vector Double
  let !va100 = fromList [1 ..] :: VNH.Vector 100 Double
  -- let !va100' = fromList [1 ..] :: A.Array '[100] Double
  -- let !vh100 = H.fromList [1 :: H.R .. 100]

  -- rv100 <- tdotv vv100
  ra100 <- tdota va100
  -- ra100' <- tdota va100'
  -- rh100 <- tdoth vh100

  putStrLn $ ("vector dot " :: Text) <> show (sum (fst ra100))

  pure ()
\end{code}

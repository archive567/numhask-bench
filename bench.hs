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
import qualified NumHask.Array.Dynamic as D
import qualified NumHask.Array.Fixed as F
import qualified NumHask.Array.HMatrix as H
import NumHask.Prelude as NH
import qualified Numeric.LinearAlgebra as HMatrix
import Options.Generic
import Perf
import Perf.Analysis
import qualified Protolude as P
import Readme.Lhs

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
  let !n = 100
  _ <- warmup 100
  let ma x = ticks n (x `F.mmult`) x
  let mnad x = ticks n (x `D.mmult`) x
  let mnah x = ticks n (x `H.mmult`) x
  let mh x = ticks n (x HMatrix.<>) x
  let !ma10 = [1 .. 100] :: F.Array '[10, 10] Double
  let !mnah10 = [1 .. 100] :: H.Array '[10, 10] Double
  let !mnad10 = D.fromFlatList [10, 10] [1 .. 100] :: D.Array Double
  let !mh10 = (10 HMatrix.>< 10) [1 :: HMatrix.R ..]
  rma10 <- ma ma10
  drma10 <- mnad (F.toDynamic ma10)
  hrma10 <- mnah mnah10
  rmh10 <- mh mh10
  rmnah10 <- mnah mnah10
  rmnad10 <- mnad mnad10
  putStrLn ("mmult 10x10" :: Text)
  putStrLn $ ("hmatrix " :: Text) <> formatF 2 (percentile 0.5 (fst rmh10))
  putStrLn $ ("Fixed " :: Text) <> formatF 2 (percentile 0.5 (fst rma10))
  putStrLn $ ("HMatrix " :: Text) <> formatF 2 (percentile 0.5 (fst rmnah10))
  putStrLn $ ("Dynamic " :: Text) <> formatF 2 (percentile 0.5 (fst rmnad10))

  r2 <- dot100 2
  r100 <- dot100 100

  void
    $ runOutput
      ("readme_.md", GitHubMarkdown)
      ("readme.md", GitHubMarkdown)
    $ do
      output "values" $ Native $ (: []) $
        bool
          (table mempty [] [] [] [[Text.pack . show <$> fst $ r100]])
          (Para [Str "results agree"])
          (and $ (==) <$> fst r100 <*> fst r100)
      output "inner" $
        Native
          [ formatRunsMedian
              ["2", "100"]
              (zip ["NumHask.Array.Fixed", "NumHask.Array.Dynamic", "vector"] $ P.transpose [snd r2, snd r100])
          ]
      output "mmult" $
        Native
          [ formatRunsMedian
              ["10"]
              [ ("NumHask.Array.Fixed", [fst rma10]),
                ("NumHask.Array.Dynamic", [fst drma10]),
                ("NumHask.Array.HMatrix", [fst hrma10]),
                ("Numeric.LinearAlgebra.R", [fst rmh10])
              ]
          ]

tdotnaf n x = ticks n (F.dot sum (+) x) x
tdotnad n x = ticks n (D.dot sum (+) x) x
tdotv n x = ticks n (sum . V.zipWith (*) x) x

dot100 :: Int -> IO ([Double], [[Cycle]])
dot100 n = do
  let !vnaf = fromList [1 .. 100] :: F.Array '[100] Double
  let !vnad = D.fromFlatList [100] [1 .. 100] :: D.Array Double
  let !vv = V.fromList [1 .. 100] :: V.Vector Double
  (pnaf, rnaf) <- tdotnaf n vnaf
  (pnad, _) <- tdotnad n vnad
  (pv, rv) <- tdotv n vv
  let vs :: [Double]
      vs = [F.fromScalar rnaf, rv]
  pure (vs, [pnaf,pnad,pv])


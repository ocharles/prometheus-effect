{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Concurrent
import Criterion
import Criterion.Main
import Prometheus
import System.Mem

data Metrics = Metrics
  { aCounter :: Counter
  , aGauge :: Gauge
  , aHistogram :: Histogram
  }

main :: IO ()
main = do
  (noopMetrics, _) <-
    buildRegistry $ do
      aCounter <- register "noop_counter" "" mempty counter
      aGauge <- register "noop_gauge" "" mempty gauge
      aHistogram <- register "noop_histo" "" mempty (histogram (linearBuckets 1 1 10))
      return Metrics {..}
  (metrics, retainedRegistry) <-
    buildRegistry $ do
      aCounter <- register "counter" "" mempty counter
      aGauge <- register "gauge" "" mempty gauge
      aHistogram <- register "histo" "" mempty (histogram (linearBuckets 1 1 10))
      return Metrics {..}
  performGC
  threadDelay 10000
  putStrLn "Running benchmarks:"
  defaultMainWith defaultConfig $
    [ makeBenchmarks "noop" noopMetrics
    , makeBenchmarks "registered" metrics
    ]
  seq retainedRegistry (return ())

makeBenchmarks groupLabel Metrics {..} =
  bgroup
    groupLabel
    [ bench "incCounter" (nfIO (replicateM_ n $ incCounter aCounter))
    , bench "incGauge" (nfIO (replicateM_ n $ incGauge aGauge))
    , bench "observe" (nfIO (replicateM_ n $ observe 9 aHistogram))
    ]
  where
    n = 10000

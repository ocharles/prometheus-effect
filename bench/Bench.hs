{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Main where

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
    [ bgroup
        "Noop metrics"
        [bench "incCounter" (nfIO (incCounter (aCounter noopMetrics)))
        ,bench "incGauge" (nfIO (incGauge (aGauge noopMetrics)))
        ,bench "observe" (nfIO (observe 9 (aHistogram noopMetrics)))
        ]
    , bgroup
        "Metrics"
        [bench "incCounter" (nfIO (incCounter (aCounter metrics)))
        ,bench "incGauge" (nfIO (incGauge (aGauge metrics)))
        ,bench "observe" (nfIO (observe 9 (aHistogram metrics)))
        ]
    ]
  seq retainedRegistry (return ())

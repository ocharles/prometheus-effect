{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Data.Monoid
import Weigh
import Prometheus
import Data.Text
import System.Mem
import Control.Concurrent

data Metrics = Metrics
  { testCounter :: Counter
  , testGauge :: Gauge
  , testHistogram :: Histogram
  , testDyns :: Text -> IO Counter
  }

main :: IO ()
main = do
  (Metrics {..}, registry) <-
    buildRegistry $
    Metrics <$> register "counter" "" mempty counter <*>
    register "gauge" "" mempty gauge <*>
    register "histogram" "" mempty (histogram (linearBuckets 0 1 10))
    <*> register "dyn" "" mempty (addLabels "k" counter)
  performGC
  threadDelay 10000
  incCounter testCounter
  -- putStrLn "Going"
  mainWith $ do
    action "incCounter" (incCounter testCounter)
    action "addCounter" (incCounterBy testCounter 10)
    action "incGauge" (incGauge testGauge)
    action "decGauge" (decGauge testGauge)
    action "resetGauge" (setGauge testGauge 10)
    action "observe" (observe 1 testHistogram)
    action "incDynNew" (testDyns "hello" >>= incCounter)
    action "incDynExisting" (testDyns "hello" >>= incCounter)
  seq registry (return ())
-- action "incCounter" (incCounter c)
-- action "addCounter" (addCounter 10 c)
-- action "incGauge" (incGaugeBy 1 g)
-- action "decGauge" (decGaugeBy 1 g)
-- action "resetGauge" (resetGauge 10 g)
-- action "observe" (observe 1 h)

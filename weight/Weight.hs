{-# OPTIONS_GHC -ddump-simpl #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Data.Monoid
import Weigh
import Prometheus

data Metrics = Metrics { testCounter :: Counter
                       , testGauge :: Gauge
                       , testHistogram :: Histogram
                       }

main :: IO ()
main = do
  (Metrics {..}, _) <-
    buildRegistry $
    Metrics <$> register "counter" "" mempty counter <*>
    register "gauge" "" mempty gauge <*>
    register "histogram" "" mempty (histogram (linearBuckets 0 1 10))
  mainWith $ do
    action "incCounter" (incCounter testCounter)
    action "addCounter" (incCounterBy testCounter 10)
    action "incGauge" (incGauge testGauge)
    action "decGauge" (decGauge testGauge)
    action "resetGauge" (setGauge testGauge 10)
    action "observe" (observe 1 testHistogram)
-- action "incCounter" (incCounter c)
-- action "addCounter" (addCounter 10 c)
-- action "incGauge" (incGaugeBy 1 g)
-- action "decGauge" (decGaugeBy 1 g)
-- action "resetGauge" (resetGauge 10 g)
-- action "observe" (observe 1 h)

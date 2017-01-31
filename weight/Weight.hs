{-# OPTIONS_GHC -ddump-simpl #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Data.Monoid
import Weigh
import Prometheus

data Metrics = Metrics { testCounter :: Metric Static Counter
                       , testGauge :: Metric Static Gauge
                       , testHistogram :: Metric Static Histogram
                       }

main :: IO ()
main = do
  (Metrics {..}, _) <-
    buildRegistry $
    Metrics <$> newMetric "counter" "" mempty Counter <*>
    newMetric "gauge" "" mempty Gauge <*>
    newMetric "histogram" "" mempty (Histogram (linearBuckets 0 1 10))
  withMetric testCounter $ \c ->
    withMetric testGauge $ \g ->
      withMetric testHistogram $ \h ->
        mainWith $ do
          action "incCounter" (incCounter c)
          action "addCounter" (addCounter 10 c)
          action "incGauge" (incGaugeBy 1 g)
          action "decGauge" (decGaugeBy 1 g)
          action "resetGauge" (resetGauge 10 g)
          action "observe" (observe 1 h)

          -- action "incCounter" (incCounter c)
          -- action "addCounter" (addCounter 10 c)
          -- action "incGauge" (incGaugeBy 1 g)
          -- action "decGauge" (decGaugeBy 1 g)
          -- action "resetGauge" (resetGauge 10 g)
          -- action "observe" (observe 1 h)

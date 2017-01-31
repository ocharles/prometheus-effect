{-# LANGUAGE ApplicativeDo, RecordWildCards, OverloadedStrings #-}
module Main where

import System.Random
import Control.Concurrent
import Control.Applicative
import Data.Monoid
import Network.Wai
import Network.Wai.Handler.Warp
import Prometheus
import Network.HTTP.Types

type StaticCounter = Metric Static Counter
type StaticHistogram = Metric Static Histogram

data Metrics = Metrics { counter :: StaticCounter
                       , selfTimer :: StaticHistogram
                       }

main :: IO ()
main = do
  Metrics {..} <-
    pushMetrics "localhost" 80 "/metrics/job/test/instance/foo" $
    register
      (do counter <-
            newMetric "ollie_requests_served_total" "Testing" mempty Counter
          selfTimer <-
            newMetric
              "ollie_requests_duration_seconds5"
              ""
              mempty
              (Histogram (exponentialBuckets 0.0000000001 10 10))
          return Metrics {..})
  run 5811 $ \req respond -> do
    withMetric counter $ \c -> withMetric selfTimer $ time $ incCounter c
    respond $ responseLBS status200 [] "Hello World"

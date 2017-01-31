{-# LANGUAGE ApplicativeDo, RecordWildCards, OverloadedStrings #-}
module Reg where

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

reg :: IO Metrics
reg = do
  (middleware, metrics) <-
    registerToMiddleware ["metrics"] $ do
      register
        (do counter <-
              newMetric "ollie_requests_served_total" "Testing" mempty Counter
            selfTimer <-
              newMetric
                "ollie_requests_duration_seconds4"
                ""
                mempty
                (Histogram (exponentialBuckets 0.0000000001 10 10))
            return Metrics {..})
  return metrics
{-# NOINLINE reg #-}

{-# LANGUAGE ApplicativeDo, RecordWildCards, OverloadedStrings #-}
module Main where

import System.Random
import Control.Concurrent
import Control.Applicative
import Data.Monoid
import Network.Wai
import Network.Wai.Handler.Warp
import Prometheus
import Prometheus.GHC
import Network.HTTP.Types

data Metrics = Metrics
  { testcounter :: Counter
  , testhistogram :: Histogram
  , testcounterTimer :: Histogram
  , testhistogramTimer :: Histogram
  }

main :: IO ()
main = do
  ((Metrics {..}, pusher), registry) {- collectGHCStats, -}
     <-
    buildRegistry $ do
      testcounter <- register "prom_test_counter" "Testing" mempty counter
      testhistogram <-
        register
          "prom_test_histo"
          ""
          mempty
          (histogram (exponentialBuckets 1e-12 10 10))
      testcounterTimer <-
        register
          "prom_incCounter"
          ""
          mempty
          (histogram (exponentialBuckets 1e-12 10 10))
      testhistogramTimer <-
        register
          "prom_observe"
          ""
          mempty
          (histogram (exponentialBuckets 1e-12 10 10))
      -- collectGHCStats <- ghcStats
      pusher <- pushMetrics "localhost" 9091 "/metrics/job/j/instance/i"
      return (Metrics {..}, pusher) {- collectGHCStats, -}
  -- collectGHCStats
  pusher registry
  run 5811 $ \req respond
                  -- publishRegistryMiddleware ["metrics"] registry $ \req respond -> do
   -> do
    time testcounterTimer $ incCounter testcounter
    time testhistogramTimer $ observe 1 testhistogram
    respond $ responseLBS status200 [] "Hello World"

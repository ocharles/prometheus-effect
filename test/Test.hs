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
import Data.Text

data Metrics = Metrics
  { testcounter :: Text -> IO Counter
  , testhistogram :: Histogram
  , testcounterTimer :: Histogram
  , testhistogramTimer :: Histogram
  }

main :: IO ()
main = do
  (launchStatsCollection, ghcStatsRegistry) <- buildRegistry ghcStats
  launchStatsCollection
  (launchPusher, pushRegistry) <-
    buildRegistry (pushMetrics "localhost" 9091 "/metrics/job/j/instance/i")
  (Metrics {..}, myRegistry) <-
    buildRegistry $ do
      testcounter <-
        register
          "prom_test_counter"
          "Testing"
          mempty
          (addLabels "test_label" counter)
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
      return Metrics {..}
  launchPusher (myRegistry <> ghcStatsRegistry <> pushRegistry)
  run 5811 $ \req respond
                  -- publishRegistryMiddleware ["metrics"] registry $ \req respond -> do
   -> do
    time testcounterTimer $ do
      incCounter =<< testcounter "foo"
      incCounter =<< testcounter "bar"
    time testhistogramTimer $ observe 1 testhistogram
    respond $ responseLBS status200 [] "Hello World"

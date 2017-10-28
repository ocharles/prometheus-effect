{-# LANGUAGE ApplicativeDo, RecordWildCards, OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Concurrent
import Data.Foldable
import Data.Monoid
import Data.Text
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Prometheus
import Prometheus.GHC
import System.Random

data Metrics = Metrics
  { testcounter :: Text -> IO Counter
  , testhistogram :: Histogram
  , testcounterTimer :: Histogram
  , testhistogramTimer :: Histogram
  }

main :: IO ()
main = do
  (launchStatsCollection, ghcStatsRegistry) <- buildRegistry ghcStats
  for_ launchStatsCollection id
  (launchPusher, pushRegistry) <-
    buildRegistry (pushMetrics "http://localhost:9091/metrics/job/j/instance/i")
  (Metrics {..}, myRegistry) <-
    buildRegistry $ do
      testcounter <-
        register
          "prom_test_counter"
          "Testing"
          mempty
          (addLabel "test_label" counter)
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
  let registry = myRegistry <> ghcStatsRegistry <> pushRegistry
  launchPusher registry
  run 5811 $
    publishRegistryMiddleware ["metrics"] registry $ \req respond -> do
      time testcounterTimer $ do
        incCounter =<< testcounter "foo"
        incCounter =<< testcounter "bar"
      time testhistogramTimer $ observe 1 testhistogram
      respond $ responseLBS status200 [] "Hello World"

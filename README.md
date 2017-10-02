[Prometheus](https://prometheus.io) is an open-source systems monitoring and
alerting toolkit originally built at SoundCloud. Since its inception in 2012,
many companies and organizations have adopted Prometheus, and the project has a
very active developer and user community. It is now a standalone open source
project and maintained independently of any company. To emphasize this and
clarify the project's governance structure, Prometheus joined the Cloud Native
Computing Foundation in 2016 as the second hosted project after Kubernetes.

This library provides a Haskell client to Prometheus. It supports:

* The metric types __counter__, __gauge__ and __histogram__.
* Publishing metrics over HTTP (via WAI middleware).
* Pushing metrics to the Prometheus push gateway.
* Labels, along with dynamic labels.
* Instrumentation, both for internal Prometheus monitoring and GHC statistics.

The library is intended to be easy to use, because instrumentation is already
boring enough - the last thing you want is to be 5 pages deep in obscure GHC
extensions just to bump a counter!

Here's one example to demonstrate how you could use this library:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Concurrent
import Control.Monad
import Network.HTTP.Types.Status
import Prometheus
import System.Random
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

data Metrics = Metrics
  { iterations :: Counter
  , timePerLoop :: Histogram
  }

main :: IO ()
main = do
  (metrics, registry) <- buildRegistry $ do
    iterations <- register "iterations" "Total completed iterations" mempty counter
    timePerLoop <- register "time_per_loop" "Distribution of time per loop" mempty (histogram ioDurationBuckets)
    return Metrics{..}

  forkIO $ Warp.run 8000 $ publishRegistryMiddleware ["metrics"] registry $ \req mkRes ->
    mkRes (Wai.responseLBS notFound404 mempty mempty)

  forever $ time (timePerLoop metrics) $ do
    threadDelay =<< randomIO
    incCounter (iterations metrics)
```

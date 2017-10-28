{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.Prometheus (instrumentRequests) where

import Control.Monad.State (MonadState)
import Control.Monad.IO.Class (MonadIO)
import qualified Network.Wai as Wai
import Prometheus 
  
--------------------------------------------------------------------------------
{-|

Build some very simple middleware that registers some HTTP related metrics:

* @http_requests_total@: Total number of HTTP requests.
* @http_latency_seconds@: Overall HTTP transaction latency.

-}
instrumentRequests
  :: (MonadState Registry m, MonadIO m) => m Wai.Middleware
instrumentRequests = do
  httpRequestsTotal <-
    register
      "http_requests_total"
      "Total number of HTTP requests."
      mempty
      counter
  httpLatency <-
    register
      "http_latency_seconds"
      "Overall HTTP transaction latency."
      mempty
      (histogram ioDurationBuckets)
  return $ \app req res -> do
    incCounter httpRequestsTotal
    time httpLatency $ app req res

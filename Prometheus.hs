{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Prometheus
  ( -- * Creating & Registering Metrics
    register
  , MetricName
  , MetricHelp
  , Metric
  , addLabels
  , Registry
    -- * Publishing Metrics
  , publishRegistryMiddleware
  , pushMetrics
  , buildRegistry

    -- * Metric Types
    -- ** Counters
  , Counter
  , counter
  , incCounter
  , incCounterBy
  , countExceptions

    -- ** Gauges
  , Gauge
  , gauge
  , incGauge
  , decGauge
  , adjustGauge
  , setGauge

    -- ** Histograms
  , Histogram
  , histogram
  , Buckets
  , linearBuckets, exponentialBuckets
  , observe
  , time

    -- * Instrumentation
  , instrumentRequests
  ) where

import Control.Applicative
import Control.Concurrent (forkIO, threadDelay, ThreadId)
import Control.Concurrent.MVar
       (newMVar, modifyMVar, modifyMVar_, readMVar, withMVar)
import Control.Exception.Safe (MonadMask, onException, finally)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State.Strict (StateT(..), runStateT)
import Control.Retry (recoverAll, capDelay, exponentialBackoff)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, doubleDec)
import Data.Foldable (for_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Hashable
import Data.IORef
import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.String (IsString)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8Builder, decodeUtf8)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import GHC.Float (Double(..))
import qualified Network.HTTP.Types as HTTP
import Network.Http.Client
       (withConnection, buildRequest, http, sendRequest, Method(POST),
        openConnection, Hostname, Port)
import qualified Network.Wai as Wai
import Numeric.Natural (Natural)
import Prelude hiding (sum)
import System.Clock
import System.IO.Streams (write)

--------------------------------------------------------------------------------
-- | 'Metric' describes a specific type of metric, but in an un-registered
-- state. Pass 'Metric's to 'registerMetric' to construct the metric for
-- operation.
newtype Metric a =
  Metric (IO (a, IO [Sample]), MetricType)

class ToLabelMap a where
  toLabelMap :: a -> a -> HashMap Text Text

instance ToLabelMap Text where
  toLabelMap k v = Map.singleton k v

addLabels :: ToLabelMap a => a -> Metric m -> Metric (a -> IO m)
addLabels keys (Metric (io, t)) = Metric (dynamic, t)
  where
    dynamic = do
      children <- newMVar mempty
      return (retrieveFrom children, sampleChildren children)
      where
        sampleChildren ref = do
          children <- readMVar ref
          Map.foldlWithKey'
            (\m labels (_, sample) ->
               liftA2
                 (++)
                 m
                 (fmap
                    (map (\s -> s {sampleLabels = sampleLabels s <> labels}))
                    sample))
            (return [])
            children
        retrieveFrom ref values = do
          modifyMVar ref $ \children -> do
            let lbls = toLabelMap keys values
            case Map.lookup lbls children of
              Just (child, _) -> do
                return (children, child)
              Nothing -> do
                (a, sample) <- io
                let !children' = Map.insert lbls (a, sample) children
                return $! (children', a)

--------------------------------------------------------------------------------
-- Metric metadata

newtype MetricName = MetricName Text
  deriving (Ord, Eq, IsString, Hashable)

newtype MetricHelp = MetricHelp Text
  deriving (IsString)


--------------------------------------------------------------------------------
data Sample = Sample
  { sampleName :: !Text
  , sampleLabels :: !(HashMap Text Text)
  , sampleValue :: {-# UNPACK #-}!Double
  } deriving (Show)

--------------------------------------------------------------------------------
newtype Registry = Registry (HashMap MetricName RegisteredMetric)
  deriving (Monoid)

data MetricType
  = TCounter
  | THistogram
  | TGauge

data RegisteredMetric = RegisteredMetric
  { metricHelp :: {-# UNPACK #-} !MetricHelp
  , metricType :: !MetricType
  , metricSample :: IO [Sample]
  }

data RegistrationFailure = MetricCollision
  { collidingName :: MetricName
  }

register
  :: MetricName
  -> MetricHelp
  -> HashMap Text Text
  -> Metric a
  -> StateT Registry IO a
register name help labels (Metric (constructor, t)) =
  StateT $ \(Registry registered) ->
    case Map.lookup name registered of
      Just _ -> undefined -- return (Left MetricCollision {collidingName = name})
      Nothing -> do
        (a, sample) <- constructor
        return $
          ( a
          , Registry
              (Map.insert
                 name
                 (RegisteredMetric
                  { metricHelp = help
                  , metricType = t
                  , metricSample =
                      (fmap
                         (map
                            (\s -> s {sampleLabels = sampleLabels s <> labels}))
                         sample)
                  })
                 registered))

buildRegistry :: StateT Registry m a -> m (a, Registry)
buildRegistry m = runStateT m (Registry mempty)

--------------------------------------------------------------------------------
newtype Counter = Counter (Double -> IO ())

counter :: Metric Counter
counter =
  Metric
    ( do counterRef <- newIORef 0
         return
           ( Counter (\d -> modifyIORef' counterRef (+ d))
           , pure . Sample "" mempty <$> readIORef counterRef)
    , TCounter)

incCounter :: Counter -> IO ()
incCounter = flip incCounterBy 1
{-# INLINE incCounter #-}

incCounterBy :: Counter -> Double -> IO ()
incCounterBy (Counter f) = f
{-# INLINE incCounterBy #-}

countExceptions
  :: Counter -> IO a -> IO a
countExceptions c m = m `onException` incCounter c
{-# INLINE countExceptions #-}


--------------------------------------------------------------------------------
newtype Gauge =
  Gauge ((Double -> Double) -> IO ())

gauge :: Metric Gauge
gauge =
  Metric
    ( do ref <- newIORef 0
         return
           ( Gauge (modifyIORef' ref)
           , pure . Sample "" mempty <$> readIORef ref)
    , TCounter)

incGauge :: MonadIO m => Gauge -> m ()
incGauge = flip adjustGauge (+ 1)
{-# INLINE incGauge #-}

decGauge :: MonadIO m => Gauge -> m ()
decGauge = flip adjustGauge (subtract 1)
{-# INLINE decGauge #-}

setGauge :: MonadIO m => Gauge -> Double -> m ()
setGauge g = adjustGauge g . const
{-# INLINE setGauge #-}

adjustGauge :: MonadIO m => Gauge -> (Double -> Double) -> m ()
adjustGauge (Gauge f) = liftIO . f
{-# INLINE adjustGauge #-}


--------------------------------------------------------------------------------
data SumAndCount = SumAndCount
  { sum :: {-# UNPACK #-}!Double
  , count :: {-# UNPACK #-}!Double
  }

instance Monoid SumAndCount where
  mempty = SumAndCount 0 0
  SumAndCount a b `mappend` SumAndCount c d = SumAndCount (a + c) (b + d)

newtype Histogram = Histogram (Double -> IO ())

newtype Buckets = Buckets (Vector Double)

-- | @linearBuckets start width numBuckets@ creates @numBuckets@ buckets, each
-- @width@ wide, where the lowest bucket has an upper bound of @start@
-- (assuming @width@ is positive).
linearBuckets :: Double -> Double -> Natural -> Buckets
linearBuckets start width numBuckets =
  Buckets
    (V.prescanl' (+) start (V.replicate (fromIntegral numBuckets) width) <>
     V.singleton (read "Infinity"))

-- @exponentialBuckets @start factor numBuckets@ creates @numBuckets@ buckets,
-- where the lowest bucket has an upper bound of @start@ and each following
-- bucket's upper bound is @factor@ times the previous bucket's upper bound.
exponentialBuckets :: Double -> Double -> Natural -> Buckets
exponentialBuckets start factor numBuckets
  | start > 0 && factor > 1 =
    Buckets
      (V.prescanl' (*) start (V.replicate (fromIntegral numBuckets) factor) <>
       V.singleton (read "Infinity"))
  | otherwise = error "Invalid arguments"

histogram :: Buckets -> Metric Histogram
histogram (Buckets v) =
  Metric
    ( do counts <- MV.replicate (V.length v) (0 :: Double)
         sumAndCount <- newMVar mempty
         return
           ( Histogram (observeImpl sumAndCount counts)
           , sample sumAndCount counts)
    , THistogram)
  where
    observeImpl sumAndCount observations a = do
      let i = V.findIndex (a <=) v
      seq i $
        modifyMVar_ sumAndCount $ \(SumAndCount s count) -> do
          for_ i (MV.unsafeModify observations succ)
          return $! SumAndCount (s + a) (count + 1)
    sample sumAndCount observations = do
      (obs, SumAndCount {..}) <-
        withMVar sumAndCount $ \sumCount -> do
          obs <- V.freeze observations
          return (obs, sumCount)
      let countSamples =
            V.ifoldl'
              (\xs i n ->
                 Sample "" (Map.singleton "le" (pack (show (v V.! i)))) n : xs)
              []
              (V.postscanl' (+) 0 obs)
          sumSample = Sample "_sum" mempty sum
          countSample = Sample "_count" mempty count
      return (sumSample : countSample : countSamples)

observe :: MonadIO m => Double -> Histogram -> m ()
observe a (Histogram f) = liftIO (f a)
{-# INLINE observe #-}

time
  :: (MonadIO m, MonadMask m)
  => Histogram -> m a -> m a
time h m = do
  t0 <- liftIO (getTime Monotonic)
  m `finally`
    (do t <- liftIO (getTime Monotonic)
        let delta = fromIntegral (toNanoSecs (t - t0)) * 1e-9
        observe delta h)
{-# INLINE time #-}


--------------------------------------------------------------------------------
pushMetrics
  :: Hostname
  -> Port
  -> ByteString
  -> StateT Registry IO (Registry -> IO ThreadId)
pushMetrics host port path = do
  let labels =
        Map.fromList
          [ ("host", decodeUtf8 host)
          , ("port", pack (show port))
          , ("path", decodeUtf8 "path")
          ]
  pushLatency <-
    register
      "haskell_prometheus_push_latency_seconds"
      "The latency when pushing metrics to a Pushgateway"
      labels
      (histogram (exponentialBuckets 1e-6 10 7))
  pushInterval <-
    register
      "haskell_prometheus_push_interval_seconds"
      "The interval between pushes"
      labels
      (histogram (linearBuckets 1 1 10))
  pushExceptions <-
    register
      "haskell_prometheus_push_exceptions_total"
      "Total count of exceptions while pushing metrics"
      labels
      counter
  return $ \reg ->
    liftIO $
    forkIO $ do
      req <- buildRequest $ http POST path
      recoverAll (capDelay 60000000 $ exponentialBackoff 500000) $ \_ ->
        countExceptions pushExceptions $
        withConnection (openConnection host port) $ \c ->
          forever $
          time pushInterval $ do
            t0 <- getTime Monotonic
            time pushLatency $
              sendRequest
                c
                req
                (\out -> writeRegistry reg (\b -> write (Just b) out))
            t <- getTime Monotonic
            let delta = fromIntegral (toNanoSecs (t - t0)) * 1e-3 :: Double
                delay = round (5e6 - delta)
            threadDelay delay


--------------------------------------------------------------------------------
writeRegistry :: Registry -> (Builder -> IO ()) -> IO ()
writeRegistry (Registry reg) emit = do
  Map.foldlWithKey'
    (\m k v -> do
       writeMetric emit k v
       emit "\n"
       m)
    (return ())
    reg
  emit "\n"

writeMetric :: (Builder -> IO ()) -> MetricName -> RegisteredMetric -> IO ()
writeMetric out (MetricName metricName) RegisteredMetric { metricHelp = MetricHelp metricHelp
                                                         , metricType = t
                                                         , ..
                                                         } = do
  let nameBuilder = encodeUtf8Builder metricName
  out ("# HELP " <> nameBuilder <> " " <> encodeUtf8Builder metricHelp <> "\n")
  out ("# TYPE " <> nameBuilder <> " " <> encodeMetricType t <> "\n")
  samples <- metricSample
  for_ samples $ \Sample {..} ->
    out
      (nameBuilder <> encodeUtf8Builder sampleName <> encodeLabels sampleLabels <>
       " " <>
       doubleDec sampleValue <>
       "\n")
  where
    encodeMetricType TCounter = "counter"
    encodeMetricType TGauge = "gauge"
    encodeMetricType THistogram = "histogram"
    encodeLabels m
      | Map.null m = ""
      | otherwise =
        "{" <> mconcat (intersperse "," (fmap buildLabelPair (Map.toList m))) <>
        "}"
    buildLabelPair (k, v) =
      encodeUtf8Builder k <> "=\"" <> encodeUtf8Builder v <> "\""


--------------------------------------------------------------------------------
instrumentRequests
  :: StateT Registry IO Wai.Middleware
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
      (histogram (exponentialBuckets 0.001 5 10))
  return $ \app req res -> do
    incCounter httpRequestsTotal
    time httpLatency $ app req res


--------------------------------------------------------------------------------
publishRegistryMiddleware
  :: [Text] -> Registry -> Wai.Middleware
publishRegistryMiddleware path reg app req respond =
  if Wai.requestMethod req == HTTP.methodGet && Wai.pathInfo req == path
    then respond (respondWithMetrics reg)
    else app req respond

respondWithMetrics :: Registry -> Wai.Response
respondWithMetrics reg =
  Wai.responseStream HTTP.status200 [] $ \emit flush -> do
    writeRegistry reg emit
    flush

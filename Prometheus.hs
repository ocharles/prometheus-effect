{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Prometheus
  ( -- * Creating & Registering Metrics
    register
  , Metric
  , addLabels

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

import Prelude hiding (sum)
import Control.Applicative
import Control.Exception.Safe (MonadMask, onException, finally)
import Control.Concurrent (forkIO, threadDelay, ThreadId)
import Control.Concurrent.MVar
       (MVar, newMVar, modifyMVar, modifyMVar_, readMVar, withMVar)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State (StateT(..), runStateT)
import Control.Retry (recoverAll, capDelay, exponentialBackoff)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, doubleDec)
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Int (Int64)
import Data.List (intersperse)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import Data.String (IsString)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8Builder, decodeUtf8)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as MV
import GHC.Float (Double(..))
import Network.Http.Client
       (withConnection, buildRequest, http, sendRequest, Method(POST),
        openConnection, Hostname, Port)
import Numeric.Natural (Natural)
import System.Clock
import System.IO.Streams (write)
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP

--------------------------------------------------------------------------------
-- | 'Metric' describes a specific type of metric, but in an un-registered
-- state. Pass 'Metric's to 'registerMetric' to construct the metric for
-- operation.
newtype Metric a = Metric (IO (a, IO [Sample]), MetricType)

class ToLabelMap a where
  toLabelMap :: a -> a -> Map Text Text

instance ToLabelMap Text where
  toLabelMap k v = Map.singleton k v

addLabels :: ToLabelMap a => a -> Metric m -> Metric (a -> IO m)
addLabels keys (Metric (io, t)) = Metric (dynamic, t)
  where
    dynamic = do
      children <- newMVar mempty
      return (retrieveFrom children, sampleChildren children)
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
          Just (child, _) -> return (children, child)
          Nothing -> do
            (a, sample) <- io
            return (Map.insert lbls (a, sample) children, a)

--------------------------------------------------------------------------------
-- Metric metadata

newtype MetricName = MetricName Text
  deriving (Ord, Eq, IsString)

newtype MetricHelp = MetricHelp Text
  deriving (IsString)


--------------------------------------------------------------------------------
data Sample = Sample
  { sampleName :: !Text
  , sampleLabels :: !(Map Text Text)
  , sampleValue :: {-# UNPACK #-}!Double
  }

--------------------------------------------------------------------------------
newtype Registry = Registry (Map MetricName RegisteredMetric)

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
  -> Map Text Text
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
newtype Counter = Counter (IORef Double)

counter :: Metric Counter
counter =
  Metric
    ( do counterRef <- newIORef 0
         return
           ( Counter counterRef
           , pure . Sample "" mempty <$> readIORef counterRef)
    , TCounter)

incCounter :: MonadIO m => Counter -> m ()
incCounter = flip incCounterBy 1
{-# INLINE incCounter #-}

incCounterBy :: MonadIO m => Counter -> Double -> m ()
incCounterBy (Counter ioRef) delta = liftIO (modifyIORef' ioRef (+ delta))
{-# INLINE incCounterBy #-}

countExceptions
  :: (MonadIO m, MonadMask m)
  => Counter -> m a -> m a
countExceptions c m = m `onException` incCounter c
{-# INLINE countExceptions #-}


--------------------------------------------------------------------------------
newtype Gauge = Gauge (IORef Double)

gauge :: Metric Gauge
gauge =
  Metric
    ( do ref <- newIORef 0
         return (Gauge ref, pure . Sample "" mempty <$> readIORef ref)
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

adjustGauge :: MonadIO m => Gauge -> (Double -> Double) ->m ()
adjustGauge (Gauge ioRef) f = liftIO (modifyIORef' ioRef f)
{-# INLINE adjustGauge #-}


--------------------------------------------------------------------------------
data SumAndCount = SumAndCount
  { sum :: {-# UNPACK #-}!Double
  , count :: {-# UNPACK #-}!Double
  }

instance Monoid SumAndCount where
  mempty = SumAndCount 0 0
  SumAndCount a b `mappend` SumAndCount c d = SumAndCount (a + c) (b + d)

data Histogram = Histogram
  { histogramObservations :: {-# UNPACK #-} !(IOVector Int64)
    -- ^ The count of observations in a given bucket. Bucket
    -- upper-bounds correspond to 'histogramBucketBounds'.
  , histogramBucketBounds :: {-# UNPACK #-} !(Vector Double)
    -- ^ An ascending vector of inclusive upper-bounds.
  , histogramSumAndCount :: {-# UNPACK #-} !(MVar SumAndCount)
    -- ^ The total sum of all observed values.
  }

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
    ( do counts <- MV.replicate (V.length v) 0
         sumAndCount <- newMVar mempty
         let sample = do
               (obs, SumAndCount {..}) <-
                 withMVar sumAndCount $ \sumCount -> do
                   obs <- V.freeze counts
                   return (obs, sumCount)
               let countSamples =
                     V.ifoldl'
                       (\xs i n ->
                          Sample
                            ""
                            (Map.singleton "le" (pack (show (v V.! i))))
                            (fromIntegral n) :
                          xs)
                       []
                       (V.postscanl' (+) 0 obs)
                   sumSample = Sample "_sum" mempty sum
                   countSample = Sample "_count" mempty count
               return (sumSample : countSample : countSamples)
         return (Histogram counts v sumAndCount, sample)
    , THistogram)

observe :: MonadIO m => Double -> Histogram -> m ()
observe a Histogram{..} = liftIO $ do
  let i = V.findIndex (a <=) histogramBucketBounds
  seq i $
    modifyMVar_ histogramSumAndCount $ \(SumAndCount s count) -> do
      for_ i (MV.unsafeModify histogramObservations succ)
      return $! SumAndCount (s + a) (count + 1)
{-# INLINE observe #-}

time
  :: (MonadIO m, MonadMask m)
  => Histogram -> m a -> m a
time histogram m = do
  t0 <- liftIO (getTime Monotonic)
  m `finally`
    (do t <- liftIO (getTime Monotonic)
        let delta = fromIntegral (toNanoSecs (t - t0)) * 1e-9
        observe delta histogram)
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
      (histogram (exponentialBuckets 0.001 2 10))
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

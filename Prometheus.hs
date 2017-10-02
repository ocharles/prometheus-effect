{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|

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

@
\{\-# LANGUAGE RecordWildCards \#\-\}

import Control.Concurrent
import qualified Network.Wai.Handler.Warp as Warp
import "Prometheus"
import System.Random

data Metrics = Metrics
  { iterations :: 'Counter'
  , timePerLoop :: 'Counter'
  }

main :: IO ()
main = do
  (metrics, registry) <- 'buildRegistry' $ do
    iterations <- 'register' "iterations" "Total completed iterations" mempty 'counter'
    timePerLoop <- 'register' "time_per_loop" "Distribution of time per loop" mempty ('histogram' 'ioDurationBuckets')

  forkIO $ Warp.run 8000 $ 'publishRegistryMiddleware' ["metrics"] registry $ \\req mkRes ->
    Warp.mkRes (Warp.responseLBS 404 mempty mempty)

  forever $ time ('timePerLoop' metrics) $ do
    threadDelay =<< randomIO
    'incCounter' (iterations metrics)
@

-}
module Prometheus
  ( -- * Creating & Registering Metrics
    register
  , RegistrationFailure(..)
  , MetricName
  , MetricHelp
  , Metric
  , addLabel

    -- ** Registries
  , Registry
  , buildRegistry

    -- * Publishing Metrics
  , publishRegistryMiddleware
  , pushMetrics
  
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
  , linearBuckets, exponentialBuckets, ioDurationBuckets, mkBuckets
  , observe
  , time
  ) where

import Control.Concurrent (forkIO, threadDelay, ThreadId)
import Control.Concurrent.MVar
       (newMVar, modifyMVar, modifyMVar_, readMVar, withMVar)
import Control.Exception.Safe (Exception, MonadMask, onException, finally, throwM)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State.Strict (StateT(..), runStateT)
import Control.Monad.State (MonadState, get, put)
import Control.Retry (recoverAll, capDelay, exponentialBackoff)
import Data.ByteString (ByteString) 
import qualified Data.ByteString.Streaming as S (fromChunks)
import qualified Data.ByteString.Streaming.HTTP as HTTP
import Data.Foldable (for_, toList)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Hashable
import Data.IORef
import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.String (IsString)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector.Algorithms.Merge as V
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import GHC.Float (Double(..))
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Network.Wai.Streaming (streamingResponse)
import Numeric.Natural (Natural)
import Prelude hiding (sum)
import Streaming (Stream, Of)
import qualified Streaming.Prelude as S
import System.Clock

--------------------------------------------------------------------------------
-- | 'Metric' describes a specific type of metric, but in an un-registered
-- state. Pass 'Metric's to 'registerMetric' to construct the metric for
-- operation.
newtype Metric a =
  Metric (IO (a, Stream (Of Sample) IO ()), MetricType)
  
-- | Append a dynamic label to a metric. A dynamic label is a label that can
-- be assigned different values at runtime. The returned metric is a function
-- from label value to a configured metric. The child metric inherits all of
-- the parent metric's labels.
--
-- As an example of where you might
-- use dynamic labels, consider the metric @http_responses_total@, which counts
-- HTTP responses. If you want to track the status code, you can add a dynamic
-- label @code@:
--
-- @
-- httpResponsesByCode <-
--   register "http_responses_total" "HTTP responses" mempty
--     (addLabel "code" counter)
--
-- -- later
--
-- okResponses <- httpResponsesByCode "200"
-- incCounter okResponses
-- @
addLabel
  :: Text -> Metric metric -> Metric (Text -> IO metric)
addLabel key (Metric (io, t)) = Metric (dynamic, t)
  where
    dynamic = do
      children <- newMVar mempty
      return (retrieveFrom children, sampleChildren children)

    sampleChildren ref = do
      children <- liftIO (readMVar ref)
      Map.foldlWithKey'
        (\out (k, v) (_, samples) -> do
           () <-
             S.map
               (\sample -> sample {sampleLabels = sampleLabels sample <> Map.singleton k v })
               samples
           out)
        (return ())
        children

    retrieveFrom ref value = do
      modifyMVar ref $ \children ->
        case Map.lookup (key, value) children of
          Just (child, _) -> do
            return (children, child)

          Nothing -> do
            (a, sample) <- io
            let !children' = Map.insert (key, value) (a, sample) children
            return $! (children', a)

--------------------------------------------------------------------------------
-- Metric metadata

-- | The metric name specifies the general feature of a system that is measured
-- (e.g. @http_requests_total@ - the total number of HTTP requests received).
-- It may contain ASCII letters and digits, as well as underscores and colons.
-- It must match the regex @[a-zA-Z_:][a-zA-Z0-9_:]*@.
--
-- Note that 'MetricName' has a 'IsString' instance. If you enable
-- @{-\# LANGUAGE OverloadedStrings #-}@, you can simply use string literals.
--
-- See also, the [best practice for metric and label naming](https://prometheus.io/docs/practices/naming/).
newtype MetricName = MetricName Text
  deriving (Ord, Eq, IsString, Hashable, Show)

-- | User readable help text describing what a given metric represents.
newtype MetricHelp = MetricHelp Text
  deriving (IsString, Show)


--------------------------------------------------------------------------------
data Sample = Sample
  { sampleName :: !Text
  , sampleLabels :: !(HashMap Text Text)
  , sampleValue :: {-# UNPACK #-}!Double
  } deriving (Show)

--------------------------------------------------------------------------------
-- | A 'Registry' is used to build up the list of all known 'Metric's that will
-- be reported to Prometheus. You add a @Metric@ to the @Registry@ with
-- 'register'.
newtype Registry = Registry (HashMap MetricName RegisteredMetric)
  deriving (Monoid)

data MetricType
  = TCounter
  | THistogram
  | TGauge

data RegisteredMetric = RegisteredMetric
  { metricHelp :: {-# UNPACK #-} !MetricHelp
  , metricType :: !MetricType
  , metricSample :: Stream (Of Sample) IO ()
  }

-- | Failures encountered when attempting to register a metric.
data RegistrationFailure =
  -- | A metric with the given name has already been registered.
  MetricCollision
    { collidingName :: MetricName -- ^ The name of the colliding metric.
    }
  deriving (Show)

instance Exception RegistrationFailure

-- | Register a metric with a 'Registry'.
--
-- Throws:
--
-- * 'RegistrationFailure'
register
  :: (MonadState Registry m, MonadIO m)
  => MetricName
     -- ^ The name of this metric.
  -> MetricHelp
     -- ^ Descriptive text about what this metric measures.
  -> HashMap Text Text
     -- ^ A map of /static/ labels, that will be applied whenever the metric is sampled.
     -- For dynamic labels (labels that change), see 'addLabel'. 
  -> Metric a
     -- ^ The metric to register.
  -> m a
register name help labels (Metric (constructor, t)) = do
  Registry registered <- get
  case Map.lookup name registered of
    Just _ -> liftIO (throwM MetricCollision {collidingName = name})
    Nothing -> do
      (a, sample) <- liftIO constructor
      put $ Registry
        (Map.insert
           name
           (RegisteredMetric
            { metricHelp = help
            , metricType = t
            , metricSample =
                (S.map
                   (\s -> s {sampleLabels = sampleLabels s <> labels})
                   sample)
            })
           registered)
      return a
        

-- | Given a computation that registers metrics, assemble a final registry.
--
-- This is really just 'runStateT', but with a name that has more meaning.
buildRegistry :: StateT Registry m a -> m (a, Registry)
buildRegistry m = runStateT m (Registry mempty)

--------------------------------------------------------------------------------
{-|

A counter is a cumulative metric that represents a single numerical value that
only ever goes up. A counter is typically used to count requests served, tasks
completed, errors occurred, etc. Counters should not be used to expose current
counts of items whose number can also go down, e.g. the number of currently
running threads. Use gauges for this use case.

-} 
newtype Counter = Counter (Double -> IO ())

-- | Create a new counter.
counter :: Metric Counter
counter =
  Metric
    ( do counterRef <- newIORef 0
         return
           ( Counter (\d -> modifyIORef' counterRef (+ d))
           , liftIO (readIORef counterRef) >>= S.yield . Sample "" mempty)
    , TCounter)

-- | Increment a counter by 1.
incCounter :: MonadIO m => Counter -> m ()
incCounter = flip incCounterBy 1
{-# INLINE incCounter #-}

-- | Increment a counter by a particular amount.
incCounterBy
  :: MonadIO m
  => Counter
  -> Double -- ^ How much to increment a counter by. /Note/: negative values will be discarded.
  -> m ()
incCounterBy (Counter f) n | signum n >= 0 = liftIO ( f n)
                           | otherwise = return ()
{-# INLINE incCounterBy #-}

-- | Instrument some code that may throw exceptions by counting the amount
-- of exceptions thrown. The exception will not be caught, only counted.
countExceptions
  :: (MonadMask m, MonadIO m) => Counter -> m a -> m a
countExceptions c m = m `onException` incCounter c
{-# INLINE countExceptions #-}


--------------------------------------------------------------------------------
{-|

A gauge is a metric that represents a single numerical value that can arbitrarily
go up and down.

Gauges are typically used for measured values like temperatures or current memory
usage, but also "counts" that can go up and down, like the number of running
threads.

-}
newtype Gauge =
  Gauge ((Double -> Double) -> IO ())

-- | Create a new gauge, initialising it at 0.
gauge :: Metric Gauge
gauge =
  Metric
    ( do ref <- newIORef 0
         return
           ( Gauge (modifyIORef' ref)
           , liftIO (readIORef ref) >>= S.yield . Sample "" mempty)
    , TCounter)

-- | Increment a gauge by 1.
incGauge :: MonadIO m => Gauge -> m ()
incGauge = flip adjustGauge (+ 1)
{-# INLINE incGauge #-}

-- | Decrement a gauge by 1.
decGauge :: MonadIO m => Gauge -> m ()
decGauge = flip adjustGauge (subtract 1)
{-# INLINE decGauge #-}

-- | Set a gauge to an exact value.
setGauge :: MonadIO m => Gauge -> Double -> m ()
setGauge g = adjustGauge g . const
{-# INLINE setGauge #-}

-- | Apply a function to move a gauge.
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

{-|

A histogram samples observations (usually things like request durations or
response sizes) and counts them in configurable buckets. It also provides a
sum of all observed values.

-}
newtype Histogram = Histogram (Double -> IO ())

-- | The buckets (bins) that a 'Histrogram' can sample to.
newtype Buckets = Buckets (Vector Double)

-- | Combining buckets concatenates the series of buckets.
instance Monoid Buckets where
  mempty = Buckets mempty
  Buckets a `mappend` Buckets b = Buckets (V.modify V.sort (a <> b))

-- | Construct 'Buckets' from anything list-like containing bucket upper-bounds.
--
-- The input list will be sorted, and does not need to be sorted before hand.
mkBuckets :: Foldable f => f Double -> Buckets
mkBuckets =
  Buckets . V.modify V.sort . V.fromList . filter (/= read "Infinity") . toList

-- | @linearBuckets start width numBuckets@ creates @numBuckets@ buckets, each
-- @width@ wide, where the lowest bucket has an upper bound of @start@
-- (assuming @width@ is positive).
linearBuckets :: Double -> Double -> Natural -> Buckets
linearBuckets start width numBuckets =
  Buckets (V.prescanl' (+) start (V.replicate (fromIntegral numBuckets) width))

-- | @exponentialBuckets start factor numBuckets@ creates @numBuckets@ buckets,
-- where the lowest bucket has an upper bound of @start@ and each following
-- bucket's upper bound is @factor@ times the previous bucket's upper bound.
exponentialBuckets :: Double -> Double -> Natural -> Buckets
exponentialBuckets start factor numBuckets
  | start > 0 && factor > 1 =
    Buckets
      (V.prescanl' (*) start (V.replicate (fromIntegral numBuckets) factor))
  | otherwise = error "Invalid arguments"

-- | Pre-defined buckets that are probably suitable for IO operations.
-- Upper-bounds are: 1μs, 10μs, 100μs, 1ms, 10ms, 100ms, 200ms, 300ms,
-- 400ms, 500ms, 600ms, 700ms, 800ms, 900ms, 1s, 2s, 4s, 8s, 16s.
ioDurationBuckets :: Buckets
ioDurationBuckets =
  exponentialBuckets 1e-6 10 5 <>
  linearBuckets 0.1 0.1 9 <>
  exponentialBuckets 1 2 5

-- | Create a new histogram that samples into a specific set of buckets.
histogram :: Buckets -> Metric Histogram
histogram buckets =
  Metric
    ( do counts <- MV.replicate (V.length v) (0 :: Double)
         sumAndCount <- newMVar mempty
         return
           ( Histogram (observeImpl sumAndCount counts)
           , sample sumAndCount counts)
    , THistogram)
  where
    v =
      case buckets of
        Buckets v' -> v' <> V.singleton (read "Infinity")
    observeImpl sumAndCount observations a = do
      let i = V.findIndex (a <=) v
      seq i $
        modifyMVar_ sumAndCount $ \(SumAndCount s count) -> do
          for_ i (MV.unsafeModify observations succ)
          return $! SumAndCount (s + a) (count + 1)
    sample sumAndCount observations = do
      (obs, SumAndCount {..}) <-
        liftIO $
        withMVar sumAndCount $ \sumCount -> do
          obs <- V.freeze observations
          return (obs, sumCount)
      let countSamples =
            V.imapM_
              (\i n ->
                 S.yield
                   (Sample "" (Map.singleton "le" (pack (show (v V.! i)))) n))
              (V.postscanl' (+) 0 obs)
          sumSample = Sample "_sum" mempty sum
          countSample = Sample "_count" mempty count
      S.yield sumSample
      S.yield countSample
      countSamples

-- | Record an observation into the buckets of a histogram.
observe :: MonadIO m => Double -> Histogram -> m ()
observe a (Histogram f) = liftIO (f a)
{-# INLINE observe #-}

-- | Run a computation, recording how long it takes to a 'Histogram'.
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
-- | Fork a thread that collects information about the instrumentation process
-- of this application itself. Specifically, the following metrics will be
-- added:
--
-- * @haskell_prometheus_push_latency_seconds@: The latency when pushing metrics to a Pushgateway
-- * @haskell_prometheus_push_interval_seconds@: The interval between pushes
-- * @haskell_prometheus_push_exceptions_total@: Total count of exceptions while pushing metrics
pushMetrics
  :: MonadIO m
  => Text -> StateT Registry IO (Registry -> m ThreadId)
pushMetrics endpoint = do
  let labels = Map.fromList [("endpoint", endpoint)]
  pushLatency <-
    register
      "haskell_prometheus_push_latency_seconds"
      "The latency when pushing metrics to a Pushgateway"
      labels
      (histogram ioDurationBuckets)
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
    forkIO $
    recoverAll (capDelay 60000000 $ exponentialBackoff 500000) $ \_ ->
      countExceptions pushExceptions $ do
        m <- HTTP.newManager HTTP.tlsManagerSettings
        req <- HTTP.parseRequest (unpack endpoint)
        let req' =
              req
              { HTTP.method = "POST"
              , HTTP.requestBody =
                  HTTP.stream (S.fromChunks (streamRegistry reg))
              }
        forever $
          time pushInterval $ do
            t0 <- getTime Monotonic
            time pushLatency $ HTTP.withHTTP req' m $ \_resp -> return ()
            t <- getTime Monotonic
            let delta = fromIntegral (toNanoSecs (t - t0)) * 1e-3 :: Double
                delay = round (5e6 - delta)
            threadDelay delay


--------------------------------------------------------------------------------
streamRegistry :: Registry -> Stream (Of ByteString) IO ()
streamRegistry (Registry reg) =
  S.map encodeUtf8 $
  Map.foldlWithKey'
    (\stream k v -> do
       streamMetric k v
       stream)
    (return ())
    reg

streamMetric :: MetricName -> RegisteredMetric -> Stream (Of Text) IO ()
streamMetric (MetricName metricName) RegisteredMetric { metricHelp = MetricHelp metricHelp
                                                      , metricType = t
                                                      , ..
                                                      } = do
  S.yield ("# HELP " <> metricName <> " " <> metricHelp <> "\n")
  S.yield ("# TYPE " <> metricName <> " " <> metricTypeText t <> "\n")
  S.map sampleToText metricSample
  where
    sampleToText Sample {..} =
      metricName <> sampleName <> labelsToText sampleLabels <> " " <>
      pack (show sampleValue) <> "\n"
    metricTypeText TCounter = "counter"
    metricTypeText TGauge = "gauge"
    metricTypeText THistogram = "histogram"
    labelsToText m
      | Map.null m = ""
      | otherwise =
        "{" <> mconcat (intersperse "," (fmap labelPairText (Map.toList m))) <>
        "}"
    labelPairText (k, v) =
      k <> "=\"" <> v <> "\""


--------------------------------------------------------------------------------
-- | Build WAI middleware that responds to @GET@ requests to @path@ by streaming
-- Prometheus metrics. This is the typical way to expose instrumentation,
-- allowing Prometheus to collect metrics directly from the app by polling.
publishRegistryMiddleware
  :: [Text] -> Registry -> Wai.Middleware
publishRegistryMiddleware path reg app req respond =
  if Wai.requestMethod req == HTTP.methodGet && Wai.pathInfo req == path
    then respond (respondWithMetrics reg)
    else app req respond

respondWithMetrics :: Registry -> Wai.Response
respondWithMetrics reg = streamingResponse (streamRegistry reg) HTTP.status200 []

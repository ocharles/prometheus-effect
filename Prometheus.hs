{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs, NamedFieldPuns, RecordWildCards,
  MultiParamTypeClasses, FlexibleInstances, OverloadedStrings, ViewPatterns, TypeFamilies #-}

module Prometheus
  ( -- * Creating & Registering Metrics
    -- ** Creating
    newMetric
  , newDynamicMetric
  , MetricOpts(..)
  , MetricName
  , MetricHelp
  , StaticLabels

    -- ** Registering
  , Unregistered
  , register

    -- * Updating Metrics
  , withMetric
  , withDynamicMetric
  , SuppliesValues

    -- * Publishing Metrics
  , registerToMiddleware
  , pushMetrics
  , buildRegistry

    -- * Metric Types
  , Metric
  , Static, Dynamic

    -- ** Counters
  , Counter
  , incCounter
  , addCounter

    -- ** Gauges
  , Gauge
  , incGaugeBy
  , decGaugeBy
  , resetGauge

    -- ** Histograms
  , Histogram
  , Buckets
  , linearBuckets, exponentialBuckets
  , observe
  , time
  ) where

import GHC.Int (Int64(..))
import GHC.Float (Double(..))
import GHC.Prim (Int#, Double#, (+##), (+#), MutVar#)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
       (MVar, newMVar, modifyMVar, modifyMVar_, readMVar, withMVar)
import Control.Exception
import Control.Monad (when, forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (modify)
import Control.Monad.Trans.State (StateT, runStateT, mapStateT)
import Data.ByteString.Builder (Builder, doubleDec)
import Data.Foldable (for_)
import Data.IORef
       (IORef, newIORef, readIORef, atomicModifyIORef', writeIORef,
        atomicWriteIORef)
import Data.Int (Int64)
import Data.List (intersperse)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import Data.Sequence (Seq, (|>))
import Data.String (IsString)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8Builder)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as MV
import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits (KnownSymbol, symbolVal, symbolVal')
import Labels ((:=)(..))
import qualified Network.HTTP.Types as HTTP
import Network.Http.Client
       (withConnection, buildRequest, http, sendRequest, Method(POST),
        openConnection)
import qualified Network.Wai as Wai
import Numeric.Natural (Natural)
import System.Clock
import System.IO.Streams (write)

newtype Static a = Static a

newtype LabelKey a = LabelKey Text

instance (KnownSymbol a, a ~ b) => IsLabel a (LabelKey b) where
  fromLabel p = LabelKey (pack (symbolVal' p))

newtype MetricName = MetricName Text
  deriving (IsString)

newtype MetricHelp = MetricHelp Text
  deriving (IsString)

newtype StaticLabels = StaticLabels (Map Text Text)
  deriving (Monoid)

-- | A single metric with a given name
data Metric dynamics metricType = Metric
  { metricName :: {-# UNPACK #-} !MetricName
  , metricHelp :: {-# UNPACK #-} !MetricHelp
  , metricStaticLabels :: !StaticLabels
  , metricOpts :: !(MetricOpts metricType)
  , metricRetrieve :: !(dynamics (AMetric metricType))
  }

-- | GADT to differentiate metrics at output time.
data AMetric a where
  ACounter :: {-# UNPACK #-} !Counter -> AMetric Counter
  AHistogram :: {-# UNPACK #-} !Histogram -> AMetric Histogram
  AGauge :: {-# UNPACK #-} !Gauge -> AMetric Gauge

data MetricOpts metricType where
  Counter :: MetricOpts Counter
  Histogram :: Buckets -> MetricOpts Histogram
  Gauge :: MetricOpts Gauge

-- | Monotonically incrementing unbounded counters.
newtype Counter = CounterData { counterCount :: IORef Double }

-- newtype DynamicLabels labels = DynamicLabels labels

construct
  :: MetricOpts a -> IO (AMetric a)
construct Counter = do
  counterCount <- liftIO (newIORef 0)
  return $! ACounter CounterData {..}
construct Gauge = do
  counterCount <- liftIO (newIORef 0)
  return $! AGauge (GaugeData CounterData {..})
construct (Histogram (Buckets histogramBucketBounds)) = do
  histogramObservations <- MV.replicate (V.length histogramBucketBounds) 0
  histogramSumAndCount <- newMVar (SumAndCount (# 0.0##, 0# #))
  return $! AHistogram (HistogramData {..})

newMetric
  :: MetricName
  -> MetricHelp
  -> StaticLabels
  -> MetricOpts metricType
  -> Unregistered (Metric Static metricType)
newMetric metricName metricHelp metricStaticLabels metricOpts =
  toRegister $ do
    metric <- liftIO (construct metricOpts)
    return $! Metric {metricRetrieve = Static metric, ..}

incCounter :: MonadIO m => Counter -> m ()
incCounter = addCounter 1
{-# INLINE incCounter #-}

addCounter :: MonadIO m => Double -> Counter -> m ()
addCounter delta CounterData {counterCount} =
  liftIO (atomicModifyIORef' counterCount (\n -> (n + delta, ())))
{-# INLINE addCounter #-}

newtype Gauge = GaugeData Counter

incGaugeBy :: (MonadIO m) => Double -> Gauge -> m ()
incGaugeBy delta (GaugeData counter) = addCounter delta counter
{-# INLINE incGaugeBy #-}

decGaugeBy :: (MonadIO m) => Double -> Gauge -> m ()
decGaugeBy delta (GaugeData counter) = addCounter delta counter
{-# INLINE decGaugeBy #-}

resetGauge :: (MonadIO m) => Double -> Gauge -> m ()
resetGauge n (GaugeData CounterData {counterCount}) =
  liftIO (atomicWriteIORef counterCount n)
{-# INLINE resetGauge #-}

withMetric :: Metric Static metric -> (metric -> a) -> a
withMetric Metric {metricRetrieve} f =
  let Static metric = metricRetrieve
  in case metric of
       ACounter a -> f a
       AHistogram a -> f a
       AGauge a -> f a
{-# INLINE withMetric #-}

newtype Dynamic labelKeys a = Dynamic (MVar (Map (Map Text Text) a))

newDynamicMetric
  :: MetricName
  -> MetricHelp
  -> StaticLabels
  -> dynamicLabels
  -> MetricOpts metricType
  -> Unregistered (Metric (Dynamic dynamicLabels) metricType)
newDynamicMetric metricName metricHelp metricStaticLabels _dynamicLabels metricOpts =
  toRegister $ do
    dynamicMetrics <- liftIO (newMVar Map.empty)
    return $! Metric {metricRetrieve = Dynamic dynamicMetrics, ..}
{-# INLINE newDynamicMetric #-}

class SuppliesValues (labelKeys :: k) labelValues where
  labelMap :: Metric (Dynamic labelKeys) metric -> labelValues -> Map Text Text

instance SuppliesValues () () where
  labelMap _ _ = Map.empty

instance v ~ Text => SuppliesValues (LabelKey k) (k := v) where
  labelMap _ (p := v) = Map.singleton (pack (symbolVal p)) v

withDynamicMetric
  :: SuppliesValues labelKeys labelValues
  => Metric (Dynamic labelKeys) metric
  -> labelValues
  -> (metric -> IO b)
  -> IO b
withDynamicMetric m@Metric {..} labels f = do
  let lbls = labelMap m labels
      Dynamic dynMapIORef = metricRetrieve
  metric <-
    do modifyMVar dynMapIORef $ \dynMap -> do
         case Map.lookup lbls dynMap of
           Nothing -> do
             metric <- construct metricOpts
             return (Map.insert lbls metric dynMap, metric)
           Just metric -> return (dynMap, metric)
  case metric of
    ACounter a -> f a
    AGauge a -> f a
    AHistogram a -> f a
{-# INLINE withDynamicMetric #-}

class StreamMetric f where
  streamMetric :: (Builder -> IO ()) -> Metric f metricType -> IO ()

instance StreamMetric Static where
  streamMetric emit Metric { metricName = MetricName name
                           , metricHelp = MetricHelp help
                           , metricOpts
                           , metricStaticLabels = StaticLabels labels
                           , metricRetrieve
                           } = do
    let nameBuilder = encodeUtf8Builder name
    emit ("# HELP " <> nameBuilder <> " " <> encodeUtf8Builder help <> "\n")
    emit ("# TYPE " <> nameBuilder <> " " <> metricType <> "\n")
    let Static metric = metricRetrieve
    case metric of
      ACounter CounterData {counterCount} -> do
        n <- readIORef counterCount
        emit (nameBuilder <> " " <> doubleDec n)
      AHistogram HistogramData {..} -> do
        (obs, SumAndCount (# s, count #)) <-
          withMVar histogramSumAndCount $ \sumCount -> do
            obs <- V.freeze histogramObservations
            return (obs, sumCount)
        V.ifoldM'_
          (\acc i n -> do
             let acc' = acc + n
             emit
               (nameBuilder <>
                buildLabels
                  (fmap encodeUtf8Builder labels <>
                   Map.singleton
                     "le"
                     (doubleDec (histogramBucketBounds V.! i))) <>
                " " <>
                encodeUtf8Builder (pack (show acc')) <>
                "\n")
             return acc')
          0
          obs
        emit
          (nameBuilder <> "_sum" <> buildLabels (fmap encodeUtf8Builder labels) <>
           " " <>
           doubleDec (D# s) <>
           "\n")
        emit
          (nameBuilder <> "_count" <>
           buildLabels (fmap encodeUtf8Builder labels) <>
           " " <>
           encodeUtf8Builder (pack (show (I64# count))))
    where
      metricType =
        case metricOpts of
          Counter {} -> "counter"
          Gauge {} -> "gauge"
          Histogram {} -> "histogram"

buildLabels :: Map Text Builder -> Builder
buildLabels labels
  | Map.null labels = ""
  | otherwise =
    "{" <> mconcat (intersperse "," (fmap buildLabelPair (Map.toList labels))) <>
    "}"
  where
    buildLabelPair (k, v) = encodeUtf8Builder k <> "=\"" <> v <> "\""

instance StreamMetric (Dynamic keys) where
  streamMetric emit Metric { metricName = MetricName name
                           , metricHelp = MetricHelp help
                           , metricOpts
                           , metricRetrieve
                           } = do
    let nameBuilder = encodeUtf8Builder name
    emit ("# HELP " <> nameBuilder <> " " <> encodeUtf8Builder help <> "\n")
    emit ("# TYPE " <> nameBuilder <> " " <> metricType)
    let Dynamic dyn = metricRetrieve
    variants <- readMVar dyn
    forVariants variants $ \labels metric ->
      case metric of
        ACounter CounterData {counterCount} -> do
          n <- readIORef counterCount
          emit "\n"
          emit
            (nameBuilder <> buildLabels (fmap encodeUtf8Builder labels) <> " " <>
             doubleDec n)
        AHistogram HistogramData {..} -> do
          (obs, SumAndCount (# s, count #)) <-
            withMVar histogramSumAndCount $ \sumCount -> do
              obs <- V.freeze histogramObservations
              return (obs, sumCount)
          V.ifoldM'_
            (\acc i n ->  do
               let acc' = acc + n
               emit
                 (nameBuilder <>
                  buildLabels
                    (fmap encodeUtf8Builder labels <>
                     Map.singleton
                       "le"
                       (doubleDec (histogramBucketBounds V.! i))) <>
                  " " <>
                  encodeUtf8Builder (pack (show acc')) <>
                  "\n")
               return acc') 0
            obs
          emit
            (nameBuilder <> "_sum" <> buildLabels (fmap encodeUtf8Builder labels) <> " " <>
             doubleDec (D# s) <> "\n")
          emit
            (nameBuilder <> "_count" <> buildLabels (fmap encodeUtf8Builder labels) <>
             " " <>
             encodeUtf8Builder (pack (show (I64# count))))
    where
      metricType =
        case metricOpts of
          Counter {} -> "counter"
          Gauge {} -> "gauge"
          Histogram {} -> "histogram"

forVariants
  :: Map (Map Text Text) (AMetric metricType)
  -> (forall a. Map Text Text -> AMetric a -> IO ())
  -> IO ()
forVariants metrics f =
  Map.foldlWithKey' (\m k a -> f k a >> m) (return ()) metrics

data SumAndCount = SumAndCount (# Double#, Int# #)

data Histogram = HistogramData
  { histogramObservations :: !(IOVector Int64)
    -- ^ The count of observations in a given bucket. Bucket
    -- upper-bounds correspond to 'histogramBucketBounds'.
  , histogramBucketBounds :: !(Vector Double)
    -- ^ An ascending vector of inclusive upper-bounds.
  , histogramSumAndCount :: !(MVar SumAndCount)
    -- ^ The total sum of all observed values.
  }

newtype Buckets = Buckets (Vector Double)

-- | @linearBuckets start width numBuckets@ creates @numBuckets@ buckets, each
-- @width@ wide, where the lowest bucket has an upper bound of @start@
-- (assuming @width@ is positive).
linearBuckets :: Double -> Double -> Natural -> Buckets
linearBuckets start width numBuckets =
  Buckets
    (V.map (start +) (V.replicate (fromIntegral numBuckets) width))

-- @exponentialBuckets @start factor numBuckets@ creates @numBuckets@ buckets,
-- where the lowest bucket has an upper bound of @start@ and each following
-- bucket's upper bound is @factor@ times the previous bucket's upper bound.
exponentialBuckets :: Double -> Double -> Natural -> Buckets
exponentialBuckets start factor numBuckets
  | start > 0 && factor > 1 =
    Buckets
      (V.postscanl'
         (*)
         start
         (V.replicate (fromIntegral numBuckets) factor))
  | otherwise = error "Invalid arguments"

time
  :: IO a -> Histogram -> IO a
time m histogram =
  bracket
    (liftIO (getTime Monotonic))
    (\t0 -> do
       t <- liftIO (getTime Monotonic)
       let delta = fromIntegral (toNanoSecs (t - t0)) * 10e-9
       observe delta histogram)
    (\_t0 -> m)
{-# INLINE time #-}

observe :: Double -> Histogram -> IO ()
observe a@(D# a#) HistogramData {..} = do
  let i = V.findIndex (<= a) histogramBucketBounds
  seq i $
    modifyMVar_ histogramSumAndCount $ \(SumAndCount (# s, count #)) -> do
      for_ i (MV.unsafeModify histogramObservations succ)
      return (SumAndCount (# s +## a#, count +# 1# #))
{-# INLINE observe #-}

data AnyMetric where
  AnyMetric :: StreamMetric f => Metric f a -> AnyMetric

newtype Registry = Registry (Seq AnyMetric)

newtype RegistryT m a = RegistryT (StateT Registry m a)
  deriving (Functor, Applicative, Monad)

newtype Unregistered a = Unregistered (RegistryT IO a)
  deriving (Functor, Applicative, Monad)

buildRegistry :: RegistryT m a -> m (a, Registry)
buildRegistry (RegistryT s) = runStateT s (Registry mempty)


registerToMiddleware
  :: Monad m
  => [Text] -> RegistryT m a -> m (Wai.Middleware, a)
registerToMiddleware path (RegistryT s) = do
  (a, reg) <- runStateT s (Registry mempty)
  let middleware app req respond =
        if Wai.requestMethod req == HTTP.methodGet && Wai.pathInfo req == path
          then respond (respondWithMetrics reg)
          else app req respond
  return (middleware, a)

pushMetrics host port path (RegistryT s) = do
  (a, reg) <- runStateT s (Registry mempty)
  forkIO
    (withConnection
       (openConnection host port)
       (\c -> forever $ do
          req <- buildRequest $ http POST path
          sendRequest
            c
            req
            (\out -> outputRegistry reg (\b -> write (Just b) out))
          threadDelay 1000000))
  return a

outputRegistry :: Registry -> (Builder -> IO ()) -> IO ()
outputRegistry (Registry reg) emit = do
  for_ reg $ \(AnyMetric m) -> do
    emit "\n"
    streamMetric emit m
  emit "\n"

respondWithMetrics :: Registry -> Wai.Response
respondWithMetrics reg =
  Wai.responseStream HTTP.status200 [] $ \emit flush -> do
    outputRegistry reg emit
    flush

toRegister
  :: StreamMetric f
  => StateT Registry IO (Metric f a) -> Unregistered (Metric f a)
toRegister m = Unregistered $ RegistryT $ do
  metric <- m
  modify (\(Registry s) -> Registry (s |> AnyMetric metric))
  return metric
{-# INLINE toRegister #-}

class Monad m => MonadPrometheus m where
  register :: Unregistered a -> m a

instance MonadIO m => MonadPrometheus (RegistryT m) where
  register (Unregistered (RegistryT act)) = RegistryT (mapStateT liftIO act)

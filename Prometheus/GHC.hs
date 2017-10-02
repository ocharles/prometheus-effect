{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Prometheus.GHC (ghcStats) where

import Control.Concurrent
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState)
import Data.Monoid (mempty)
import qualified GHC.Stats as GHC
import Prometheus

newUnlabelledMetric
  :: (MonadState Registry m, MonadIO m)
  => MetricName
  -> MetricHelp
  -> Metric a
  -> m a
newUnlabelledMetric n h t = register n h mempty t

{-|

Register metrics that record internal GHC runtime metrics, and fork a new thread
to periodically update those metrics. Returns the 'ThreadId' of the monitoring
thread.

The following metrics will be registered:

* @ghc_bytes_allocated_total@: Total number of bytes allocated
* @ghc_num_gcs_total@: Number of garbage collections performed (any generation, major and minor)
* @ghc_max_bytes_used_total@: Maximum number of live bytes seen so far
* @ghc_num_byte_usage_samples_total@: Number of byte usage samples taken, or equivalently the number of major GCs performed.
* @ghc_cumulative_bytes_used_total@: Sum of all byte usage samples, can be used with numByteUsageSamples to calculate averages with arbitrary weighting (if you are sampling this record multiple times).
* @ghc_bytes_copied_total@: Number of bytes copied during GC
* @ghc_current_bytes_used@: Number of live bytes at the end of the last major GC
* @ghc_current_bytes_slop@: Current number of bytes lost to slop
* @ghc_max_bytes_slop_total@: Maximum number of bytes lost to slop at any one time so far
* @ghc_peak_bytes_allocated_total@: Maximum number of bytes allocated
* @ghc_mutator_cpu_seconds_total@: CPU time spent running mutator threads. This does not include any profiling overhead or initialization.
* @ghc_mutator_wall_seconds_total@: Wall clock time spent running mutator threads. This does not include initialization.
* @ghc_gc_cpu_seconds_total@: CPU time spent running GC
* @ghc_gc_wall_seconds_total@: Wall clock time spent running GC
* @ghc_cpu_seconds_total@: Total CPU time elapsed since program start
* @ghc_wall_seconds_total@: Total wall clock time elapsed since start
* @ghc_par_bytes_copied_total@: Number of bytes copied during GC, minus space held by mutable lists held by the capabilities. Can be used with parMaxBytesCopied to determine how well parallel GC utilized all cores.
* @ghc_par_max_bytes_copied_total@: Sum of number of bytes copied each GC by the most active GC thread each GC. The ratio of parTotBytesCopied divided by parMaxBytesCopied approaches 1 for a maximally sequential run and approaches the number of threads (set by the RTS flag -N) for a maximally parallel run.
* @ghc_prometheus_collection_time_seconds@: Amount of time spent by the Prometheus library collecting GHC statistics
* @ghc_prometheus_record_time_seconds@: Amount of time spent by the Prometheus library recording GHC statistics
-}
ghcStats :: (MonadState Registry m, MonadIO m) =>  m (IO ThreadId)
ghcStats = do
  bytesAllocated <-
    newUnlabelledMetric
      "ghc_bytes_allocated_total"
      "Total number of bytes allocated"
      gauge
  numGcs <-
    newUnlabelledMetric
      "ghc_num_gcs_total"
      "Number of garbage collections performed (any generation, major and minor)"
      gauge
  maxBytesUsed <-
    newUnlabelledMetric
      "ghc_max_bytes_used_total"
      "Maximum number of live bytes seen so far"
      gauge
  numByteUsageSamples <-
    newUnlabelledMetric
      "ghc_num_byte_usage_samples_total"
      "Number of byte usage samples taken, or equivalently the number of major GCs performed."
      gauge
  cumulativeBytesUsed <-
    newUnlabelledMetric
      "ghc_cumulative_bytes_used_total"
      "Sum of all byte usage samples, can be used with numByteUsageSamples to calculate averages with arbitrary weighting (if you are sampling this record multiple times)."
      gauge
  bytesCopied <-
    newUnlabelledMetric
      "ghc_bytes_copied_total"
      "Number of bytes copied during GC"
      gauge
  currentBytesUsed <-
    newUnlabelledMetric
      "ghc_current_bytes_used"
      "Number of live bytes at the end of the last major GC"
      gauge
  currentBytesSlop <-
    newUnlabelledMetric
      "ghc_current_bytes_slop"
      "Current number of bytes lost to slop"
      gauge
  maxBytesSlop <-
    newUnlabelledMetric
      "ghc_max_bytes_slop_total"
      "Maximum number of bytes lost to slop at any one time so far"
      gauge
  peakBytesAllocated <-
    newUnlabelledMetric
      "ghc_peak_bytes_allocated_total"
      "Maximum number of bytes allocated"
      gauge
  mutatorCpuSeconds <-
    newUnlabelledMetric
      "ghc_mutator_cpu_seconds_total"
      "CPU time spent running mutator threads. This does not include any profiling overhead or initialization."
      gauge
  mutatorWallSeconds <-
    newUnlabelledMetric
      "ghc_mutator_wall_seconds_total"
      "Wall clock time spent running mutator threads. This does not include initialization."
      gauge
  gcCpuSeconds <-
    newUnlabelledMetric
      "ghc_gc_cpu_seconds_total"
      "CPU time spent running GC"
      gauge
  gcWallSeconds <-
    newUnlabelledMetric
      "ghc_gc_wall_seconds_total"
      "Wall clock time spent running GC"
      gauge
  cpuSeconds <-
    newUnlabelledMetric
      "ghc_cpu_seconds_total"
      "Total CPU time elapsed since program start"
      gauge
  wallSeconds <-
    newUnlabelledMetric
      "ghc_wall_seconds_total"
      "Total wall clock time elapsed since start"
      gauge
  parTotBytesCopied <-
    newUnlabelledMetric
      "ghc_par_bytes_copied_total"
      "Number of bytes copied during GC, minus space held by mutable lists held by the capabilities. Can be used with parMaxBytesCopied to determine how well parallel GC utilized all cores."
      gauge
  parMaxBytesCopied <-
    newUnlabelledMetric
      "ghc_par_max_bytes_copied_total"
      "Sum of number of bytes copied each GC by the most active GC thread each GC. The ratio of parTotBytesCopied divided by parMaxBytesCopied approaches 1 for a maximally sequential run and approaches the number of threads (set by the RTS flag -N) for a maximally parallel run."
      gauge
  ghcStatsTime <-
    newUnlabelledMetric
      "ghc_prometheus_collection_time_seconds"
      "Amount of time spent by the Prometheus library collecting GHC statistics"
      (histogram (exponentialBuckets 1e-12 10 10))
  ghcRecordTime <-
    newUnlabelledMetric
      "ghc_prometheus_record_time_seconds"
      "Amount of time spent by the Prometheus library recording GHC statistics"
      (histogram (exponentialBuckets 1e-12 10 10))
  return $
    liftIO $
    forkIO $ do
      enabled <- GHC.getGCStatsEnabled
      case enabled of
        False -> putStrLn "GHC Statisics are not enabled."
        True ->
          forever $ do
            stats <- time ghcStatsTime GHC.getGCStats
            time ghcRecordTime $ do
              setGauge bytesAllocated (fromIntegral $ GHC.bytesAllocated stats)
              setGauge numGcs (fromIntegral $ GHC.numGcs stats)
              setGauge maxBytesUsed (fromIntegral $ GHC.maxBytesUsed stats)
              setGauge
                numByteUsageSamples
                (fromIntegral $ GHC.numByteUsageSamples stats)
              setGauge
                cumulativeBytesUsed
                (fromIntegral $ GHC.cumulativeBytesUsed stats)
              setGauge bytesCopied (fromIntegral $ GHC.bytesCopied stats)
              setGauge
                currentBytesUsed
                (fromIntegral $ GHC.currentBytesUsed stats)
              setGauge
                currentBytesSlop
                (fromIntegral $ GHC.currentBytesSlop stats)
              setGauge maxBytesSlop (fromIntegral $ GHC.maxBytesSlop stats)
              setGauge
                peakBytesAllocated
                (fromIntegral $ GHC.peakMegabytesAllocated stats * 1000000)
              setGauge mutatorCpuSeconds (GHC.mutatorCpuSeconds stats)
              setGauge mutatorWallSeconds (GHC.mutatorWallSeconds stats)
              setGauge gcCpuSeconds (GHC.gcCpuSeconds stats)
              setGauge gcWallSeconds (GHC.gcWallSeconds stats)
              setGauge cpuSeconds (GHC.cpuSeconds stats)
              setGauge wallSeconds (GHC.wallSeconds stats)
              setGauge
                parTotBytesCopied
                (fromIntegral $ GHC.parTotBytesCopied stats)
              setGauge
                parMaxBytesCopied
                (fromIntegral $ GHC.parMaxBytesCopied stats)
            threadDelay 1000000

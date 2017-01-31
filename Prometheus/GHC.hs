{-# LANGUAGE OverloadedStrings #-}

module Prometheus.GHC where

import Control.Monad (forever)
import Control.Concurrent
import Data.Monoid (mempty)
import qualified GHC.Stats as GHC
import Prometheus

newUnlabelledMetric
  :: MetricName
  -> MetricHelp
  -> MetricOpts metricType
  -> Unregistered (Metric Static metricType)
newUnlabelledMetric n h t = newMetric n h mempty t

ghcStats :: Unregistered (IO ThreadId)
ghcStats = do
  bytesAllocated <-
    newUnlabelledMetric
      "ghc_bytes_allocated_total"
      "Total number of bytes allocated"
      Gauge
  numGcs <-
    newUnlabelledMetric
      "ghc_num_gcs_total"
      "Number of garbage collections performed (any generation, major and minor)"
      Gauge
  maxBytesUsed <-
    newUnlabelledMetric
      "ghc_max_bytes_used_total"
      "Maximum number of live bytes seen so far"
      Gauge
  numByteUsageSamples <-
    newUnlabelledMetric
      "ghc_num_byte_usage_samples_total"
      "Number of byte usage samples taken, or equivalently the number of major GCs performed."
      Gauge
  cumulativeBytesUsed <-
    newUnlabelledMetric
      "ghc_cumulative_bytes_used_total"
      "Sum of all byte usage samples, can be used with numByteUsageSamples to calculate averages with arbitrary weighting (if you are sampling this record multiple times)."
      Gauge
  bytesCopied <-
    newUnlabelledMetric
      "ghc_bytes_copied_total"
      "Number of bytes copied during GC"
      Gauge
  currentBytesUsed <-
    newUnlabelledMetric
      "ghc_current_bytes_used"
      "Number of live bytes at the end of the last major GC"
      Gauge
  currentBytesSlop <-
    newUnlabelledMetric
      "ghc_current_bytes_slop"
      "Current number of bytes lost to slop"
      Gauge
  maxBytesSlop <-
    newUnlabelledMetric
      "ghc_max_bytes_slop_total"
      "Maximum number of bytes lost to slop at any one time so far"
      Gauge
  peakBytesAllocated <-
    newUnlabelledMetric
      "ghc_peak_bytes_allocated_total"
      "Maximum number of bytes allocated"
      Gauge
  mutatorCpuSeconds <-
    newUnlabelledMetric
      "ghc_mutator_cpu_seconds_total"
      "CPU time spent running mutator threads. This does not include any profiling overhead or initialization."
      Gauge
  mutatorWallSeconds <-
    newUnlabelledMetric
      "ghc_mutator_wall_seconds_total"
      "Wall clock time spent running mutator threads. This does not include initialization."
      Gauge
  gcCpuSeconds <-
    newUnlabelledMetric
      "ghc_gc_cpu_seconds_total"
      "CPU time spent running GC"
      Gauge
  gcWallSeconds <-
    newUnlabelledMetric
      "ghc_gc_wall_seconds_total"
      "Wall clock time spent running GC"
      Gauge
  cpuSeconds <-
    newUnlabelledMetric
      "ghc_cpu_seconds_total"
      "Total CPU time elapsed since program start"
      Gauge
  wallSeconds <-
    newUnlabelledMetric
      "ghc_wall_seconds_total"
      "Total wall clock time elapsed since start"
      Gauge
  parTotBytesCopied <-
    newUnlabelledMetric
      "ghc_par_bytes_copied_total"
      "Number of bytes copied during GC, minus space held by mutable lists held by the capabilities. Can be used with parMaxBytesCopied to determine how well parallel GC utilized all cores."
      Gauge
  parMaxBytesCopied <-
    newUnlabelledMetric
      "ghc_par_max_bytes_copied_total"
      "Sum of number of bytes copied each GC by the most active GC thread each GC. The ratio of parTotBytesCopied divided by parMaxBytesCopied approaches 1 for a maximally sequential run and approaches the number of threads (set by the RTS flag -N) for a maximally parallel run."
      Gauge
  ghcStatsTime <-
    newUnlabelledMetric
      "ghc_prometheus_collection_time_seconds"
      "Amount of time spent by the Prometheus library collecting GHC statistics"
      (Histogram (exponentialBuckets 1e-12 10 10))
  ghcRecordTime <-
    newUnlabelledMetric
      "ghc_prometheus_record_time_seconds"
      "Amount of time spent by the Prometheus library recording GHC statistics"
      (Histogram (exponentialBuckets 1e-12 10 10))
  return $
    forkIO $ do
      enabled <- GHC.getGCStatsEnabled
      case enabled of
        False -> putStrLn "GHC Statisics are not enabled."
        True ->
          forever $ do
            stats <- withMetric ghcStatsTime (time GHC.getGCStats)
            withMetric ghcRecordTime $
              time $ do
                withMetric bytesAllocated $
                  resetGauge (fromIntegral $ GHC.bytesAllocated stats)
                withMetric numGcs $ resetGauge (fromIntegral $ GHC.numGcs stats)
                withMetric maxBytesUsed $
                  resetGauge (fromIntegral $ GHC.maxBytesUsed stats)
                withMetric numByteUsageSamples $
                  resetGauge (fromIntegral $ GHC.numByteUsageSamples stats)
                withMetric cumulativeBytesUsed $
                  resetGauge (fromIntegral $ GHC.cumulativeBytesUsed stats)
                withMetric bytesCopied $
                  resetGauge (fromIntegral $ GHC.bytesCopied stats)
                withMetric currentBytesUsed $
                  resetGauge (fromIntegral $ GHC.currentBytesUsed stats)
                withMetric currentBytesSlop $
                  resetGauge (fromIntegral $ GHC.currentBytesSlop stats)
                withMetric maxBytesSlop $
                  resetGauge (fromIntegral $ GHC.maxBytesSlop stats)
                withMetric peakBytesAllocated $
                  resetGauge
                    (fromIntegral $ GHC.peakMegabytesAllocated stats * 1000000)
                withMetric mutatorCpuSeconds $
                  resetGauge (GHC.mutatorCpuSeconds stats)
                withMetric mutatorWallSeconds $
                  resetGauge (GHC.mutatorWallSeconds stats)
                withMetric gcCpuSeconds $ resetGauge (GHC.gcCpuSeconds stats)
                withMetric gcWallSeconds $ resetGauge (GHC.gcWallSeconds stats)
                withMetric cpuSeconds $ resetGauge (GHC.cpuSeconds stats)
                withMetric wallSeconds $ resetGauge (GHC.wallSeconds stats)
                withMetric parTotBytesCopied $
                  resetGauge (fromIntegral $ GHC.parTotBytesCopied stats)
                withMetric parMaxBytesCopied $
                  resetGauge (fromIntegral $ GHC.parMaxBytesCopied stats)
            threadDelay 1000000

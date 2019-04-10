-- | Top-level Prometheus metrics.

module Hp.Metrics
  ( requestCounter
  ) where

import Prometheus

import qualified Prometheus.Metric.GHC


ghcMetrics :: Prometheus.Metric.GHC.GHCMetrics
ghcMetrics =
  unsafeRegister Prometheus.Metric.GHC.ghcMetrics
{-# NOINLINE ghcMetrics #-}

-- | HTTP request counter.
requestCounter :: Counter
requestCounter =
  unsafeRegister (counter (Info "request_counter" "Request counter"))
{-# NOINLINE requestCounter #-}

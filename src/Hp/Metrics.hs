-- | Top-level Prometheus metrics.

module Hp.Metrics
  ( requestCounter
  ) where

import Prometheus


-- | HTTP request counter.
requestCounter :: Counter
requestCounter =
  unsafeRegister (counter (Info "request_counter" "Request counter"))
{-# NOINLINE requestCounter #-}

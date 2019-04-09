module Hp.Handler.GetMetrics
  ( handleGetMetrics
  ) where

import Control.Effect
import Prometheus


handleGetMetrics ::
     ( Carrier sig m
     , MonadIO m
     )
  => m Text
handleGetMetrics = do
  bytes <- exportMetricsAsText
  pure (bytes ^?! strict . utf8)

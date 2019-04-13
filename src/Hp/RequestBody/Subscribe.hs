module Hp.RequestBody.Subscribe where

import Data.Aeson (FromJSON)


data SubscribeRequestBody
  = SubscribeRequestBody
  deriving stock (Generic)
  deriving anyclass (FromJSON)

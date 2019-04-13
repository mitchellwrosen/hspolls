module Hp.RequestBody.Subscribe where

import Data.Aeson (FromJSON)


data SubscribeRequestBody
  = SubscribeRequestBody
  { -- | Receive an email when a poll is created?
    pollCreated :: Bool
  } deriving stock (Generic)
    deriving anyclass (FromJSON)

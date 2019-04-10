module Hp.RequestBody.CreatePoll
  ( CreatePollRequestBody(..)
  ) where

import Hp.PollFormElement

import Data.Aeson (FromJSON)
import Data.Time  (UTCTime)


data CreatePollRequestBody
  = PollRequestBody
  { elements :: Seq PollFormElement
  , endTime :: UTCTime
  } deriving stock (Generic, Show)
    deriving anyclass (FromJSON)


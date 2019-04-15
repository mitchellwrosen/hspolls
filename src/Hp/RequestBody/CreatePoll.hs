module Hp.RequestBody.CreatePoll
  ( CreatePollRequestBody(..)
  ) where

import Hp.PollFormElement

import Data.Aeson (FromJSON)
import Data.Time  (DiffTime)


data CreatePollRequestBody
  = PollRequestBody
  { duration :: DiffTime
  , elements :: [PollFormElement]
  } deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

module Hp.Poll
  ( Poll(..)
  ) where

import Hp.PollFormElement

import Data.Aeson (FromJSON)
import Data.Time  (UTCTime)


data Poll
  = Poll
  { elements :: Seq PollFormElement
  , endTime :: UTCTime
  } deriving stock (Generic)
    deriving anyclass (FromJSON)

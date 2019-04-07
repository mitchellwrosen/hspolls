module Hp.Poll
  ( Poll(..)
  , PollId(..)
  ) where

import Hp.PollFormElement

import Data.UUID (UUID)
import Data.Aeson (FromJSON)
import Data.Time  (UTCTime)


newtype PollId
  = PollId UUID

data Poll
  = Poll
  { elements :: Seq PollFormElement
  , endTime :: UTCTime
  } deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

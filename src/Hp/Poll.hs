module Hp.Poll
  ( Poll(..)
  ) where

import Hp.PollFormElement (PollFormElement)

import Data.Time (UTCTime)


data Poll id
  = Poll
  { id :: id
  , elements :: Seq PollFormElement
  , endTime :: UTCTime
  } deriving stock (Generic)

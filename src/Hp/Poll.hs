module Hp.Poll
  ( Poll(..)
  ) where

import Hp.IsEntity        (IsEntity(..))
import Hp.PollFormElement (PollFormElement)
import Hp.PollId          (PollId)

import Data.Time (UTCTime)


data Poll
  = Poll
  { elements :: Seq PollFormElement
  , endTime :: UTCTime
  } deriving stock (Generic, Show)

instance IsEntity Poll where
  type EntityId Poll
    = PollId

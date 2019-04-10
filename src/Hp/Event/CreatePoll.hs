module Hp.Event.CreatePoll where

import Hp.Poll            (PollId)
import Hp.PollFormElement (PollFormElement)

import Data.Time (UTCTime)


-- | A poll was created.
data CreatePollEvent
  = CreatePollEvent
  { id :: PollId
  , elements :: Seq PollFormElement
  , endTime :: UTCTime
  } deriving stock (Generic, Show)

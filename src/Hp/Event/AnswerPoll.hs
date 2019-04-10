module Hp.Event.AnswerPoll where

import Hp.Entity     (Entity)
import Hp.PollAnswer (PollAnswer)


-- | A poll was answered.
newtype AnswerPollEvent
  = AnswerPollEvent
  { answer :: Entity PollAnswer
  } deriving stock (Generic, Show)

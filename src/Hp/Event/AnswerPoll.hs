module Hp.Event.AnswerPoll where

import Hp.Poll         (Poll)
import Hp.PollAnswer   (PollAnswer)
import Hp.PollAnswerId (PollAnswerId)
import Hp.PollId       (PollId)
import Hp.User         (User)
import Hp.UserId       (UserId)


-- | A poll was answered.
data AnswerPollEvent
  = AnswerPollEvent
  { answer :: PollAnswer
  , id :: PollAnswerId
  , poll :: Poll PollId
  , user :: Maybe (User UserId)
  } deriving stock (Generic, Show)

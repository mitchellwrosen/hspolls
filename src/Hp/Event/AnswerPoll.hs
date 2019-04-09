module Hp.Event.AnswerPoll where

import Hp.Poll       (PollId)
import Hp.PollAnswer (PollAnswer)
import Hp.UserId     (UserId)


data AnswerPollEvent
  = AnswerPollEvent
  { answer :: PollAnswer
  , id :: PollId
  , user :: Maybe UserId
  } deriving stock (Generic)

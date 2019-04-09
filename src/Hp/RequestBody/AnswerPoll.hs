module Hp.RequestBody.AnswerPoll where

import Hp.Poll       (PollId)
import Hp.PollAnswer (PollAnswer)

import Data.Aeson (FromJSON)


data AnswerPollRequestBody
  = AnswerPollRequestBody
  { answer :: PollAnswer
  , id :: PollId
  } deriving stock (Generic)
    deriving anyclass (FromJSON)

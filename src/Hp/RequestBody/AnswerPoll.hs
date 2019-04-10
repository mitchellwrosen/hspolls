module Hp.RequestBody.AnswerPoll where

import Hp.PollAnswer (PollAnswer)
import Hp.PollId     (PollId)

import Data.Aeson (FromJSON)


data AnswerPollRequestBody
  = AnswerPollRequestBody
  { answer :: PollAnswer
  , id :: PollId
  } deriving stock (Generic)
    deriving anyclass (FromJSON)

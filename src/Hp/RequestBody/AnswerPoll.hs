module Hp.RequestBody.AnswerPoll where

import Hp.PollItemAnswer (PollItemAnswer)

import Data.Aeson (FromJSON)


data AnswerPollRequestBody
  = AnswerPollRequestBody
  { answers :: Seq PollItemAnswer
  } deriving stock (Generic)
    deriving anyclass (FromJSON)

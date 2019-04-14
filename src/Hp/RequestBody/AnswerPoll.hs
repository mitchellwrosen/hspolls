module Hp.RequestBody.AnswerPoll
  ( AnswerPollRequestBody(..)
  ) where

import Hp.PollQuestionAnswer (PollQuestionAnswer)

import Data.Aeson (FromJSON)


data AnswerPollRequestBody
  = AnswerPollRequestBody
  { response :: Seq PollQuestionAnswer
  } deriving stock (Generic)
    deriving anyclass (FromJSON)

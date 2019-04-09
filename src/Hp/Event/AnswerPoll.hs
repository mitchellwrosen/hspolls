module Hp.Event.AnswerPoll where

import Data.Aeson (FromJSON)


data AnswerPollEvent
  = AnswerPollEvent
  deriving stock (Generic)
  deriving anyclass (FromJSON)

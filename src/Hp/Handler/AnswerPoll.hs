module Hp.Handler.AnswerPoll where

import Hp.Poll (PollId)
import Hp.Event.AnswerPoll (AnswerPollEvent(..))

import Servant (NoContent)


handleAnswerPoll ::
     PollId
  -> AnswerPollEvent
  -> m NoContent
handleAnswerPoll pollId event =
  undefined

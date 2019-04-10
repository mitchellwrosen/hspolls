module Hp.Handler.CreatePoll
  ( handleCreatePoll
  ) where

import Hp.Eff.Event        (EventEffect, emitEvent)
import Hp.Eff.ManagePoll   (ManagePoll, savePoll)
import Hp.Event.CreatePoll (CreatePollEvent(..))
import Hp.Poll             (Poll, PollId)

import Control.Effect
import Servant        (NoContent(..))


handleCreatePoll ::
     ( Carrier sig m
     , Member (EventEffect CreatePollEvent) sig
     , Member ManagePoll sig
     , MonadIO m
     )
  => Poll
  -> m NoContent
handleCreatePoll poll = do
  pollId :: PollId <-
    savePoll poll

  emitEvent CreatePollEvent
    { elements = poll ^. #elements
    , endTime = poll ^. #endTime
    , id = pollId
    }

  pure NoContent

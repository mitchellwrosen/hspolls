module Hp.Handler.CreatePoll
  ( handleCreatePoll
  ) where

import Hp.Eff.Event              (EventEffect, emitEvent)
import Hp.Eff.ManagePoll         (ManagePoll, savePoll)
import Hp.Event.CreatePoll       (CreatePollEvent(..))
import Hp.Poll                   (Poll(..))
import Hp.PollId                 (PollId)
import Hp.RequestBody.CreatePoll (CreatePollRequestBody(..))

import Control.Effect
import Servant        (NoContent(..))


handleCreatePoll ::
     ( Carrier sig m
     , Member (EventEffect CreatePollEvent) sig
     , Member ManagePoll sig
     , MonadIO m
     )
  => CreatePollRequestBody
  -> m NoContent
handleCreatePoll body = do
  pollId :: PollId <-
    savePoll Poll
      { id = ()
      , elements = body ^. #elements
      , endTime = body ^. #endTime
      }

  emitEvent CreatePollEvent
    { elements = body ^. #elements
    , endTime = body ^. #endTime
    , id = pollId
    }

  pure NoContent

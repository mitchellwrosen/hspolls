module Hp.Handler.CreatePoll
  ( handleCreatePoll
  ) where

import Hp.Eff.PersistPoll        (PersistPollEffect, savePoll)
import Hp.Eff.Yield              (YieldEffect, yield)
import Hp.Entity                 (Entity(..))
import Hp.Event.CreatePoll       (CreatePollEvent(..))
import Hp.Poll                   (Poll(..))
import Hp.PollId                 (PollId)
import Hp.RequestBody.CreatePoll (CreatePollRequestBody(..))

import Control.Effect
import Prelude        hiding (id)
import Servant        (NoContent(..))


handleCreatePoll ::
     ( Carrier sig m
     , Member (YieldEffect CreatePollEvent) sig
     , Member PersistPollEffect sig
     , MonadIO m
     )
  => CreatePollRequestBody
  -> m NoContent
handleCreatePoll body = do
  pollId :: PollId <-
    savePoll poll

  yield CreatePollEvent
    { poll = Entity pollId poll }

  pure NoContent

  where
    poll :: Poll
    poll =
      Poll
        { elements = body ^. #elements
        , endTime = body ^. #endTime
        }

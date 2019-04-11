module Hp.Handler.CreatePoll
  ( handleCreatePoll
  ) where

import Hp.Eff.PersistPoll        (PersistPollEffect, savePoll)
import Hp.Eff.Yield              (YieldEffect, yield)
import Hp.Entity                 (Entity(..))
import Hp.Event.PollCreated      (PollCreatedEvent(..))
import Hp.Poll                   (Poll(..))
import Hp.RequestBody.CreatePoll (CreatePollRequestBody(..))

import Control.Effect
import Prelude        hiding (id)
import Servant        (NoContent(..))


handleCreatePoll ::
     ( Carrier sig m
     , Member (YieldEffect PollCreatedEvent) sig
     , Member PersistPollEffect sig
     , MonadIO m
     )
  => CreatePollRequestBody
  -> m NoContent
handleCreatePoll body = do
  poll :: Entity Poll <-
    savePoll (body ^. #duration) (body ^. #elements)

  yield PollCreatedEvent
    { poll = poll }

  pure NoContent

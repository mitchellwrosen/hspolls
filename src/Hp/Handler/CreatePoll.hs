module Hp.Handler.CreatePoll
  ( handleCreatePoll
  ) where

import Hp.Eff.PersistPoll        (PersistPollEffect, savePoll)
import Hp.Eff.Yield              (YieldEffect, yield)
import Hp.Entity                 (Entity(..))
import Hp.Entity.Poll            (Poll(..))
import Hp.Entity.User            (User, UserId)
import Hp.Event.PollCreated      (PollCreatedEvent(..))
import Hp.RequestBody.CreatePoll (CreatePollRequestBody(..))

import Control.Effect
import Prelude             hiding (id)
import Servant             (NoContent(..))
import Servant.Auth.Server (AuthResult(..))


handleCreatePoll ::
     ( Carrier sig m
     , Member (YieldEffect PollCreatedEvent) sig
     , Member PersistPollEffect sig
     , MonadIO m
     )
  => AuthResult (Entity User)
  -> CreatePollRequestBody
  -> m NoContent
handleCreatePoll authResult body = do
  poll :: Entity Poll <-
    savePoll
      (body ^. #duration)
      (body ^. #elements)
      userId

  yield PollCreatedEvent
    { poll = poll }

  pure NoContent

  where
    userId :: Maybe UserId
    userId = do
      Authenticated user <- pure authResult
      pure (user ^. #key)

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
import Control.Effect.Error (throwError)
import Prelude              hiding (id)
import Servant              (NoContent(..), ServerError, err400)
import Servant.Auth.Server  (AuthResult(..))


handleCreatePoll ::
     ( Carrier sig m
     , Member (Error ServerError) sig
     , Member PersistPollEffect sig
     , Member (YieldEffect PollCreatedEvent) sig
     )
  => AuthResult (Entity User)
  -> CreatePollRequestBody
  -> m NoContent
handleCreatePoll authResult body = do
  validatePoll body

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

-- Validate a poll:
--
-- * Duration is at least 1 minute
-- * Form has at least 1 element
validatePoll ::
     ( Carrier sig m
     , Member (Error ServerError) sig
     )
  => CreatePollRequestBody
  -> m ()
validatePoll body = do
  when ((body ^. #duration) < 60) (throwError err400)
  when (null (body ^. #elements)) (throwError err400)

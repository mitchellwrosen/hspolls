module Hp.Handler.CreatePoll
  ( handleCreatePoll
  ) where

import Hp.Eff.PersistPoll        (PersistPollEffect, savePoll)
import Hp.Eff.Throw              (ThrowEffect, throw)
import Hp.Eff.Yield              (YieldEffect, yield)
import Hp.Entity                 (Entity(..))
import Hp.Entity.Poll            (Poll(..), PollId)
import Hp.Entity.User            (UserId)
import Hp.Event.PollCreated      (PollCreatedEvent(..))
import Hp.PollFormElement        (arePollFormElementsValid)
import Hp.RequestBody.CreatePoll (CreatePollRequestBody(..))

import Control.Effect
import Prelude             hiding (id)
import Servant             (ServerError, err400)
import Servant.Auth.Server (AuthResult(..))


handleCreatePoll ::
     ( Carrier sig m
     , Member PersistPollEffect sig
     , Member (ThrowEffect ServerError) sig
     , Member (YieldEffect PollCreatedEvent) sig
     )
  => AuthResult UserId
  -> CreatePollRequestBody
  -> m PollId
handleCreatePoll authResult body = do
  validatePoll body

  poll :: Entity Poll <-
    savePoll
      (body ^. #duration)
      (body ^. #elements)
      userId

  yield PollCreatedEvent
    { poll = poll }

  pure (poll ^. #key)

  where
    userId :: Maybe UserId
    userId = do
      Authenticated id <- pure authResult
      pure id

-- Validate a poll:
--
-- * Duration is at least 1 minute
-- * Questions are all valid
validatePoll ::
     ( Carrier sig m
     , Member (ThrowEffect ServerError) sig
     )
  => CreatePollRequestBody
  -> m ()
validatePoll body = do
  when ((body ^. #duration) < 60) (throw err400)
  when (not (arePollFormElementsValid (body ^. #elements))) (throw err400)

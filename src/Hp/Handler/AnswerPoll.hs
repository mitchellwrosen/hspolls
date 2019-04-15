module Hp.Handler.AnswerPoll
  ( handleAnswerPoll
  ) where

import Hp.Eff.GetCurrentTime     (GetCurrentTimeEffect)
import Hp.Eff.PersistPoll        (PersistPollEffect, getPoll)
import Hp.Eff.PersistPollAnswer  (PersistPollAnswerEffect, putPollAnswer)
import Hp.Eff.Yield              (YieldEffect, yield)
import Hp.Entity                 (Entity(..))
import Hp.Entity.Poll            (PollId, isPollExpired)
import Hp.Entity.PollAnswer      (PollAnswer(..))
import Hp.Entity.User            (User)
import Hp.Event.PollAnswered     (PollAnsweredEvent(..))
import Hp.RequestBody.AnswerPoll (AnswerPollRequestBody(..))

import Control.Effect
import Control.Effect.Error (throwError)
import Servant              (NoContent(..), ServerError, err403, err404)
import Servant.Auth.Server  (AuthResult(..))


handleAnswerPoll ::
     ( Carrier sig m
     , Member (Error ServerError) sig
     , Member GetCurrentTimeEffect sig
     , Member PersistPollEffect sig
     , Member PersistPollAnswerEffect sig
     , Member (YieldEffect PollAnsweredEvent) sig
     )
  => AuthResult (Entity User)
  -> PollId
  -> AnswerPollRequestBody
  -> m NoContent
handleAnswerPoll authResult pollId body =
  getPoll pollId >>= \case
    Nothing ->
      throwError err404

    Just poll -> do
      expired :: Bool <-
        isPollExpired (poll ^. #value)

      when expired
        (throwError err403)

      -- TODO validate poll answer

      pollAnswer :: Entity PollAnswer <-
        putPollAnswer pollId (body ^. #response) (view #key <$> user)

      yield PollAnsweredEvent
        { answer = pollAnswer }

      pure NoContent

  where
    user :: Maybe (Entity User)
    user = do
      Authenticated user <- pure authResult
      pure user

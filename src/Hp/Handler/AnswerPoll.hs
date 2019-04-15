module Hp.Handler.AnswerPoll
  ( handleAnswerPoll
  ) where

import Hp.Eff.GetCurrentTime     (GetCurrentTimeEffect)
import Hp.Eff.PersistPoll        (PersistPollEffect, getPoll)
import Hp.Eff.PersistPollAnswer  (PersistPollAnswerEffect, putPollAnswer)
import Hp.Eff.Throw              (ThrowEffect, throw)
import Hp.Eff.Yield              (YieldEffect, yield)
import Hp.Entity                 (Entity(..))
import Hp.Entity.Poll            (PollId, isPollExpired, pollQuestions)
import Hp.Entity.PollAnswer      (PollAnswer(..))
import Hp.Entity.User            (User)
import Hp.Event.PollAnswered     (PollAnsweredEvent(..))
import Hp.PollQuestionAnswer     (arePollQuestionAnswersValid)
import Hp.RequestBody.AnswerPoll (AnswerPollRequestBody(..))

import Control.Effect
import Servant             (NoContent(..), ServerError, err400, err403, err404)
import Servant.Auth.Server (AuthResult(..))


handleAnswerPoll ::
     ( Carrier sig m
     , Member GetCurrentTimeEffect sig
     , Member PersistPollEffect sig
     , Member PersistPollAnswerEffect sig
     , Member (ThrowEffect ServerError) sig
     , Member (YieldEffect PollAnsweredEvent) sig
     )
  => AuthResult (Entity User)
  -> PollId
  -> AnswerPollRequestBody
  -> m NoContent
handleAnswerPoll authResult pollId body =
  getPoll pollId >>= \case
    Nothing ->
      throw err404

    Just poll -> do
      expired :: Bool <-
        isPollExpired (poll ^. #value)

      when expired
        (throw err403)

      unless
        (arePollQuestionAnswersValid
          (pollQuestions (poll ^. #value))
          (body ^. #response))
        (throw err400)

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

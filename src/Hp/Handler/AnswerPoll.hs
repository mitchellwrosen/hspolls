module Hp.Handler.AnswerPoll where

import Hp.Eff.PersistPoll        (PersistPollEffect, getPoll)
import Hp.Eff.PersistPollAnswer  (PersistPollAnswerEffect, putPollAnswer)
import Hp.Eff.Yield              (YieldEffect, yield)
import Hp.Entity                 (Entity(..))
import Hp.Entity.Poll            (PollId)
import Hp.Entity.PollAnswer      (PollAnswer(..))
import Hp.Entity.User            (User)
import Hp.Event.PollAnswered     (PollAnsweredEvent(..))
import Hp.RequestBody.AnswerPoll (AnswerPollRequestBody(..))

import Control.Effect
import Servant             (NoContent(..))
import Servant.Auth.Server (AuthResult(..))


handleAnswerPoll ::
     ( Carrier sig m
     , Member (YieldEffect PollAnsweredEvent) sig
     , Member PersistPollEffect sig
     , Member PersistPollAnswerEffect sig
     )
  => AuthResult (Entity User)
  -> PollId
  -> AnswerPollRequestBody
  -> m NoContent
handleAnswerPoll authResult pollId body =
  getPoll pollId >>= \case
    Nothing ->
      pure NoContent -- TODO 404

    Just _poll -> do
      -- TODO check if poll is expired
      -- TODO validate poll answer

      let
        pollAnswer :: PollAnswer
        pollAnswer =
          PollAnswer
            { answers = body ^. #answers
            , pollId = pollId
            , userId = view #key <$> user
            }

      putPollAnswer pollAnswer >>= \case
        Nothing ->
          -- TODO some error code
          pure ()

        Just pollAnswerId ->
          yield PollAnsweredEvent
            { answer = Entity pollAnswerId pollAnswer }

      pure NoContent

  where
    user :: Maybe (Entity User)
    user = do
      Authenticated user <- pure authResult
      pure user

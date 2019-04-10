module Hp.Handler.AnswerPoll where

import Hp.Eff.Event              (EventEffect, emitEvent)
import Hp.Eff.ManagePoll         (ManagePoll, getPoll)
import Hp.Eff.PersistPollAnswer  (PersistPollAnswerEffect, putPollAnswer)
import Hp.Entity                 (Entity(..))
import Hp.Event.AnswerPoll       (AnswerPollEvent(..))
import Hp.PollAnswer             (PollAnswer(..))
import Hp.PollId                 (PollId)
import Hp.RequestBody.AnswerPoll (AnswerPollRequestBody(..))
import Hp.User                   (User)

import Control.Effect
import Servant             (NoContent(..))
import Servant.Auth.Server (AuthResult(..))


handleAnswerPoll ::
     ( Carrier sig m
     , Member (EventEffect AnswerPollEvent) sig
     , Member ManagePoll sig
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
          emitEvent AnswerPollEvent
            { answer = Entity pollAnswerId pollAnswer }

      pure NoContent

  where
    user :: Maybe (Entity User)
    user = do
      Authenticated user <- pure authResult
      pure user

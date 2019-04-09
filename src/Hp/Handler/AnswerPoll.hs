module Hp.Handler.AnswerPoll where

import Hp.Eff.Event              (EventEffect, emitEvent)
import Hp.Eff.ManagePoll         (ManagePoll, getPoll)
import Hp.Eff.PersistPollAnswer  (PersistPollAnswerEffect, putPollAnswer)
import Hp.Event.AnswerPoll       (AnswerPollEvent(..))
import Hp.Poll                   (PollId)
import Hp.RequestBody.AnswerPoll (AnswerPollRequestBody(..))
import Hp.User                   (User)
import Hp.UserId                 (UserId)

import Control.Effect
import Servant             (NoContent(..))
import Servant.Auth.Server (AuthResult(..))


handleAnswerPoll ::
     ( Carrier sig m
     , Member (EventEffect AnswerPollEvent) sig
     , Member ManagePoll sig
     , Member PersistPollAnswerEffect sig
     )
  => AuthResult (User UserId)
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

      putPollAnswer pollId (body ^. #answer) userId >>= \case
        Nothing ->
          -- TODO some error code
          pure ()

        Just pollAnswerId ->
          emitEvent AnswerPollEvent
            { answer = body ^. #answer
            , id = pollAnswerId
            , pollId = pollId
            , userId = userId
            }

      pure NoContent

  where
    userId :: Maybe UserId
    userId = do
      Authenticated user <- pure authResult
      pure (user ^. #id)

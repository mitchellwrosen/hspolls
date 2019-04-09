module Hp.Handler.AnswerPoll where

import Hp.Eff.ManagePoll         (ManagePoll, getPoll)
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
     , Member ManagePoll sig
     )
  => AuthResult (User UserId)
  -> PollId
  -> AnswerPollRequestBody
  -> m NoContent
handleAnswerPoll auth pollId body =
  getPoll pollId >>= \case
    Nothing ->
      pure NoContent -- TODO 404

    Just _poll -> do
      -- TODO check if poll is expired
      -- TODO validate poll answer

      let
        answerPollEvent :: AnswerPollEvent
        answerPollEvent =
          AnswerPollEvent
            { id =
                pollId
            , answer =
                body ^. #answer
            , user =
                case auth of
                  Authenticated user ->
                    Just (user ^. #id)
                  _ ->
                    Nothing
            }

      -- TODO emit answerPollEvent

      pure NoContent

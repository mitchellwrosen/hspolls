module Hp.Handler.GetPoll
  ( handleGetPoll
  ) where

import Hp.Eff.PersistPoll       (PersistPollEffect, getPoll)
import Hp.Eff.PersistPollAnswer (PersistPollAnswerEffect, getPollAnswers)
import Hp.Eff.Throw             (ThrowEffect, throw)
import Hp.Entity.Poll           (PollId)
import Hp.PollQuestionAnswer    (PollQuestionAnswer)
import Hp.ResponseBody.GetPoll  (GetPollResponseBody(..),
                                 makeGetPollResponseBody)

import Control.Effect
import Servant        (ServerError, err404)


handleGetPoll ::
     ( Carrier sig m
     , Member PersistPollEffect sig
     , Member PersistPollAnswerEffect sig
     , Member (ThrowEffect ServerError) sig
     )
  => PollId
  -> m GetPollResponseBody
handleGetPoll pollId =
  getPoll pollId >>= \case
    Nothing ->
      throw err404

    Just poll -> do
      answers :: Vector [PollQuestionAnswer] <-
        getPollAnswers (poll ^. #key)

      pure (makeGetPollResponseBody (poll ^. #value) answers)

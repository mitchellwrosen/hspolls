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
import Hp.PollQuestionAnswer     (PollQuestionAnswer,
                                  arePollQuestionAnswersValid)
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
  validateAnswerPoll pollId (body ^. #answers) >>= \case
    Left PollDoesNotExist ->
      throw err404

    Left PollIsExpired ->
      throw err403

    Left QuestionsAreInvalid ->
      throw err400

    Right () -> do
      pollAnswer :: Entity PollAnswer <-
        putPollAnswer
          (body ^. #answers)
          pollId
          (view #key <$> user)

      yield PollAnsweredEvent
        { answer = pollAnswer }

      pure NoContent

  where
    user :: Maybe (Entity User)
    user = do
      Authenticated user <- pure authResult
      pure user


data AnswerPollError
  = PollDoesNotExist
  | PollIsExpired
  | QuestionsAreInvalid

validateAnswerPoll ::
     ( Carrier sig m
     , Member GetCurrentTimeEffect sig
     , Member PersistPollEffect sig
     )
  => PollId
  -> [PollQuestionAnswer]
  -> m (Either AnswerPollError ())
validateAnswerPoll pollId answers =
  getPoll pollId >>= \case
    Nothing ->
      pure (Left PollDoesNotExist)

    Just poll -> do
      expired :: Bool <-
        isPollExpired (poll ^. #value)

      let
        valid :: Bool
        valid =
          arePollQuestionAnswersValid (pollQuestions (poll ^. #value)) answers

      pure $
        case (expired, valid) of
          (True, _) -> Left PollIsExpired
          (_, False) -> Left QuestionsAreInvalid
          _ -> Right ()

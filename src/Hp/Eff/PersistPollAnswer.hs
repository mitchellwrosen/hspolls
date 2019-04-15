module Hp.Eff.PersistPollAnswer
  ( PersistPollAnswerEffect(..)
  , getPollAnswers
  , putPollAnswer
  ) where

import Hp.Eff.FirstOrder     (FirstOrderEffect(..))
import Hp.Entity             (Entity)
import Hp.Entity.Poll        (PollId)
import Hp.Entity.PollAnswer  (PollAnswer)
import Hp.Entity.User        (UserId)
import Hp.PollQuestionAnswer (PollQuestionAnswer)

import Control.Effect
import Control.Effect.Carrier


data PersistPollAnswerEffect (m :: Type -> Type) (k :: Type) where
  GetPollAnswers ::
       PollId
    -> (Vector [PollQuestionAnswer] -> k)
    -> PersistPollAnswerEffect m k

  PutPollAnswer ::
       [PollQuestionAnswer]
    -> PollId
    -> Maybe UserId
    -> (Entity PollAnswer -> k)
    -> PersistPollAnswerEffect m k

  deriving stock (Functor)
  deriving (Effect, HFunctor)
       via (FirstOrderEffect PersistPollAnswerEffect)

-- | Get all of the answers to a poll.
getPollAnswers ::
     ( Carrier sig m
     , Member PersistPollAnswerEffect sig
     )
  => PollId
  -> m (Vector [PollQuestionAnswer])
getPollAnswers pollId =
  send (GetPollAnswers pollId pure)

-- | Insert a poll answer and return its id.
putPollAnswer ::
     ( Carrier sig m
     , Member PersistPollAnswerEffect sig
     )
  => [PollQuestionAnswer]
  -> PollId
  -> Maybe UserId
  -> m (Entity PollAnswer)
putPollAnswer answers pollId userId =
  send (PutPollAnswer answers pollId userId pure)

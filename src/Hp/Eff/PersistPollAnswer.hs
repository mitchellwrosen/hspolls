module Hp.Eff.PersistPollAnswer
  ( PersistPollAnswerEffect(..)
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
  PutPollAnswer ::
       [PollQuestionAnswer]
    -> PollId
    -> Maybe UserId
    -> (Entity PollAnswer -> k)
    -> PersistPollAnswerEffect m k

  deriving stock (Functor)
  deriving (Effect, HFunctor)
       via (FirstOrderEffect PersistPollAnswerEffect)

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

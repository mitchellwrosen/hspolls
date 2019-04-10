module Hp.Eff.PersistPollAnswer
  ( PersistPollAnswerEffect(..)
  , putPollAnswer
  ) where

import Hp.Eff.FirstOrder (FirstOrderEffect(..))
import Hp.PollAnswer     (PollAnswer)
import Hp.PollAnswerId   (PollAnswerId)
import Hp.PollId         (PollId)
import Hp.UserId         (UserId)

import Control.Effect
import Control.Effect.Carrier


data PersistPollAnswerEffect (m :: Type -> Type) (k :: Type) where
  PutPollAnswer ::
       PollId
    -> PollAnswer
    -> Maybe UserId
    -> (Maybe PollAnswerId -> k)
    -> PersistPollAnswerEffect m k

  deriving stock (Functor)
  deriving (Effect, HFunctor)
       via (FirstOrderEffect PersistPollAnswerEffect)

-- | Insert a poll answer and return its id (if successfully created).
--
-- It might fail if the poll or user were just deleted (foreign key violation).
putPollAnswer ::
     ( Carrier sig m
     , Member PersistPollAnswerEffect sig
     )
  => PollId
  -> PollAnswer
  -> Maybe UserId
  -> m (Maybe PollAnswerId)
putPollAnswer pollId pollAnswer user =
  send (PutPollAnswer pollId pollAnswer user pure)

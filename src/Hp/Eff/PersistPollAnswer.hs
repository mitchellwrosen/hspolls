module Hp.Eff.PersistPollAnswer
  ( PersistPollAnswerEffect(..)
  , putPollAnswer
  ) where

import Hp.Eff.FirstOrder (FirstOrderEffect(..))
import Hp.PollAnswer     (PollAnswer)
import Hp.PollAnswerId   (PollAnswerId)

import Control.Effect
import Control.Effect.Carrier


data PersistPollAnswerEffect (m :: Type -> Type) (k :: Type) where
  PutPollAnswer ::
       PollAnswer
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
  => PollAnswer
  -> m (Maybe PollAnswerId)
putPollAnswer pollAnswer =
  send (PutPollAnswer pollAnswer pure)

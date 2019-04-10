module Hp.Eff.PersistPoll
  ( PersistPollEffect(..)
  , getPoll
  , savePoll
  ) where

import Control.Effect
import Control.Effect.Carrier

import Hp.Entity         (Entity)
import Hp.Eff.FirstOrder (FirstOrderEffect(..))
import Hp.Poll
import Hp.PollId         (PollId(..))

data PersistPollEffect (m :: Type -> Type) (k :: Type) where
  GetPoll :: PollId -> (Maybe (Entity Poll) -> k) -> PersistPollEffect m k
  SavePoll :: Poll -> (PollId -> k) -> PersistPollEffect m k
  deriving stock (Functor)
  deriving (Effect, HFunctor)
       via (FirstOrderEffect PersistPollEffect)

savePoll :: (Carrier sig m, Member PersistPollEffect sig) => Poll -> m PollId
savePoll poll =
  send $ SavePoll poll pure

getPoll :: (Carrier sig m, Member PersistPollEffect sig) => PollId -> m (Maybe (Entity Poll))
getPoll pollId = send $ GetPoll pollId pure


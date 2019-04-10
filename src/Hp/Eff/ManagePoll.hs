-- TODO rename to PersistPoll

module Hp.Eff.ManagePoll
  ( ManagePoll(..)
  , getPoll
  , savePoll
  ) where

import Control.Effect
import Control.Effect.Carrier

import Hp.Eff.FirstOrder (FirstOrderEffect(..))
import Hp.Poll
import Hp.PollId (PollId(..))

data ManagePoll (m :: Type -> Type) (k :: Type) where
  GetPoll :: PollId -> (Maybe (Poll PollId) -> k) -> ManagePoll m k
  SavePoll :: Poll () -> (PollId -> k) -> ManagePoll m k
  deriving stock (Functor)
  deriving (Effect, HFunctor)
       via (FirstOrderEffect ManagePoll)

savePoll :: (Carrier sig m, Member ManagePoll sig) => Poll () -> m PollId
savePoll poll =
  send $ SavePoll poll pure

getPoll :: (Carrier sig m, Member ManagePoll sig) => PollId -> m (Maybe (Poll PollId))
getPoll pollId = send $ GetPoll pollId pure

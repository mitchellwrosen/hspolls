module Hp.Eff.PersistPoll
  ( PersistPollEffect(..)
  , getPoll
  , savePoll
  ) where

import Hp.Eff.FirstOrder  (FirstOrderEffect(..))
import Hp.Entity          (Entity)
import Hp.Poll
import Hp.PollFormElement (PollFormElement)
import Hp.PollId          (PollId(..))
import Hp.UserId          (UserId)

import Control.Effect
import Control.Effect.Carrier
import Data.Time              (DiffTime)


data PersistPollEffect (m :: Type -> Type) (k :: Type) where
  GetPoll ::
       PollId
    -> (Maybe (Entity Poll) -> k)
    -> PersistPollEffect m k

  SavePoll ::
       DiffTime
    -> Seq PollFormElement
    -> Maybe UserId
    -> (Entity Poll -> k)
    -> PersistPollEffect m k

  deriving stock (Functor)
  deriving (Effect, HFunctor)
       via (FirstOrderEffect PersistPollEffect)

getPoll ::
     ( Carrier sig m
     , Member PersistPollEffect sig
     )
  => PollId
  -> m (Maybe (Entity Poll))
getPoll pollId =
  send (GetPoll pollId pure)

savePoll ::
     ( Carrier sig m
     , Member PersistPollEffect sig
     )
  => DiffTime
  -> Seq PollFormElement
  -> Maybe UserId
  -> m (Entity Poll)
savePoll duration elements userId =
  send (SavePoll duration elements userId pure)

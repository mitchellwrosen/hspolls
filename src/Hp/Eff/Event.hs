module Hp.Eff.Event
  ( EventEffect(..)
  , emitEvent
  ) where

import Hp.Eff.FirstOrder (FirstOrderEffect(..))

import Control.Effect
import Control.Effect.Carrier


data EventEffect (event :: Type) (m :: Type -> Type) (k :: Type) where
  EmitEvent ::
       event
    -> k
    -> EventEffect event m k

  deriving stock (Functor)
  deriving (Effect, HFunctor)
       via (FirstOrderEffect (EventEffect event))

-- | Emit an event to anonymous upstream listeners.
emitEvent ::
     ( Carrier sig m
     , Member (EventEffect event) sig
     )
  => event
  -> m ()
emitEvent event =
  send (EmitEvent event (pure ()))

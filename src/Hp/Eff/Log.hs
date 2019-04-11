module Hp.Eff.Log
  ( LogEffect(..)
  , log
  ) where

import Hp.Eff.FirstOrder (FirstOrderEffect(..))

import Control.Effect
import Control.Effect.Carrier


data LogEffect (m :: Type -> Type) (k :: Type) where
  Log ::
       Text
    -> k
    -> LogEffect m k

  deriving stock (Functor)
  deriving (Effect, HFunctor)
       via (FirstOrderEffect LogEffect)

log ::
     ( Carrier sig m
     , Member LogEffect sig
     )
  => Text
  -> m ()
log message =
  send (Log message (pure ()))

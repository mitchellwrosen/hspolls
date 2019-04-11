module Hp.Eff.Await
  ( AwaitEffect(..)
  , await
  ) where

import Hp.Eff.FirstOrder (FirstOrderEffect(..))

import Control.Effect
import Control.Effect.Carrier


data AwaitEffect (value :: Type) (m :: Type -> Type) (k :: Type) where
  Await ::
       (value -> k)
    -> AwaitEffect value m k

  deriving stock (Functor)
  deriving (Effect, HFunctor)
       via (FirstOrderEffect (AwaitEffect value))

-- | Await a value from an anonymous producer.
await ::
     ( Carrier sig m
     , Member (AwaitEffect value) sig
     )
  => m value
await =
  send (Await pure)

module Hp.Eff.Yield
  ( YieldEffect(..)
  , yield
  ) where

import Hp.Eff.FirstOrder (FirstOrderEffect(..))

import Control.Effect
import Control.Effect.Carrier


data YieldEffect (value :: Type) (m :: Type -> Type) (k :: Type) where
  Yield ::
       value
    -> k
    -> YieldEffect value m k

  deriving stock (Functor)
  deriving (Effect, HFunctor)
       via (FirstOrderEffect (YieldEffect value))

-- | Yield a value to an anonymous listener.
yield ::
     ( Carrier sig m
     , Member (YieldEffect value) sig
     )
  => value
  -> m ()
yield value =
  send (Yield value (pure ()))

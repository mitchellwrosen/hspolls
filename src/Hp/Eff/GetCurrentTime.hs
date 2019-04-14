module Hp.Eff.GetCurrentTime
  ( GetCurrentTimeEffect(..)
  , getCurrentTime
  ) where

import Hp.Eff.FirstOrder (FirstOrderEffect(..))

import Control.Effect
import Control.Effect.Carrier
import Data.Time              (UTCTime)


data GetCurrentTimeEffect (m :: Type -> Type) (k :: Type) where
  GetCurrentTime ::
       (UTCTime -> k)
    -> GetCurrentTimeEffect m k

  deriving stock (Functor)
  deriving (Effect, HFunctor)
       via (FirstOrderEffect GetCurrentTimeEffect)

getCurrentTime ::
     ( Carrier sig m
     , Member GetCurrentTimeEffect sig
     )
  => m UTCTime
getCurrentTime =
  send (GetCurrentTime pure)

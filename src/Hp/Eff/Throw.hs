{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.Throw
  ( ThrowEffect(..)
  , throw
  , runThrow
  ) where

import Hp.Eff.FirstOrder (FirstOrderEffect(..))

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Sum

import qualified Control.Effect.Error as Error


data ThrowEffect (e :: Type) (m :: Type -> Type) (k :: Type) where
  Throw ::
       e
    -> ThrowEffect e m k

  deriving stock (Functor)
  deriving (Effect, HFunctor)
       via (FirstOrderEffect (ThrowEffect e))

throw ::
     ( Carrier sig m
     , Member (ThrowEffect e) sig
     )
  => e
  -> m a
throw err =
  send (Throw err)


newtype ThrowCarrier e m a
  = ThrowCarrier { unThrowCarrier :: Error.ErrorC e m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
     ( Carrier sig m
     , Effect sig
     )
  => Carrier (ThrowEffect e :+: sig) (ThrowCarrier e m) where

  eff ::
       (ThrowEffect e :+: sig) (ThrowCarrier e m) (ThrowCarrier e m a)
    -> ThrowCarrier e m a
  eff = \case
    L (Throw err) ->
      ThrowCarrier (Error.throwError err)

    R other ->
      ThrowCarrier (eff (R (handleCoercible other)))

runThrow ::
     ThrowCarrier e m a
  -> m (Either e a)
runThrow =
  Error.runError . unThrowCarrier

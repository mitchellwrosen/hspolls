-- | Polymorphic Control.Effect.Fail

{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.Fail
  ( Fail'(..)
  , fail'
  , FailC'
  , runFail'
  ) where

import Hp.Eff.FirstOrder (FirstOrderEffect(..))

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Error
import Control.Effect.Sum


data Fail' (e :: Type) (m :: Type -> Type) (k :: Type) where
  Fail' ::
       e
    -> Fail' e m k

  deriving stock (Functor)
  deriving (Effect, HFunctor)
       via (FirstOrderEffect (Fail' e))

fail' ::
     ( Carrier sig m
     , Member (Fail' e) sig
     )
  => e
  -> m a
fail' err =
  send (Fail' err)


newtype FailC' e m x
  = FailC' { unFailC' :: ErrorC e m x }
  deriving newtype (Applicative, Functor, Monad)

instance
     ( Carrier sig m
     , Effect sig
     )
  => Carrier (Fail' e :+: sig) (FailC' e m) where

  eff ::
       (Fail' e :+: sig) (FailC' e m) (FailC' e m a)
    -> FailC' e m a
  eff = \case
    L (Fail' err) ->
      FailC' (throwError err)

    R other ->
      FailC' (eff (R (handleCoercible other)))


runFail' ::
     FailC' e m a
  -> m (Either e a)
runFail' =
  runErrorC . unFailC'

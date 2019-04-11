module Hp.Eff.SendEmail
  ( SendEmailEffect(..)
  , sendEmail
  ) where

import Hp.Eff.FirstOrder (FirstOrderEffect(..))
import Hp.Email          (Email)

import Control.Effect
import Control.Effect.Carrier


data SendEmailEffect (m :: Type -> Type) (k :: Type) where
  SendEmail ::
       Email
    -> k
    -> SendEmailEffect m k

  deriving stock (Functor)
  deriving (Effect, HFunctor)
       via (FirstOrderEffect SendEmailEffect)

sendEmail ::
     ( Carrier sig m
     , Member SendEmailEffect sig
     )
  => Email
  -> m ()
sendEmail email =
  send (SendEmail email (pure ()))

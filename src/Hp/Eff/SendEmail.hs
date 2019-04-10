module Hp.Eff.SendEmail
  ( SendEmailEffect(..)
  , getMaxEmailRecipients
  , sendEmail
  ) where

import Hp.Eff.FirstOrder (FirstOrderEffect(..))
import Hp.Email          (Email)

import Control.Effect
import Control.Effect.Carrier


data SendEmailEffect (m :: Type -> Type) (k :: Type) where
  GetMaxEmailRecipients ::
       (Natural -> k)
    -> SendEmailEffect m k

  SendEmail ::
       Email
    -> k
    -> SendEmailEffect m k

  deriving stock (Functor)
  deriving (Effect, HFunctor)
       via (FirstOrderEffect SendEmailEffect)

-- | Return the maximum number of recipients that this email carrier will allow
-- on a single email.
getMaxEmailRecipients ::
     ( Carrier sig m
     , Member SendEmailEffect sig
     )
  => m Natural
getMaxEmailRecipients =
  send (GetMaxEmailRecipients pure)

sendEmail ::
     ( Carrier sig m
     , Member SendEmailEffect sig
     )
  => Email
  -> m ()
sendEmail email =
  send (SendEmail email (pure ()))

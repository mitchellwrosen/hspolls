module Hp.Worker.SendEmail
  ( sendEmailWorker
  ) where

import Hp.Eff.Await     (AwaitEffect, await)
import Hp.Eff.SendEmail (SendEmailEffect, getMaxEmailRecipients, sendEmail)
import Hp.Email         (Email(..))

import Control.Effect


sendEmailWorker ::
     ( Carrier sig m
     , Member (AwaitEffect Email) sig
     , Member SendEmailEffect sig
     )
  => m void
sendEmailWorker = do
  maxRecipients :: Natural <-
    getMaxEmailRecipients

  forever $ do
    email :: Email <-
      await

    case email of
      EmailPersonal{} ->
        sendEmail email

      EmailTransactional transactionalEmail ->
        if fromIntegral (length (transactionalEmail ^. #bcc)) <= maxRecipients
          then sendEmail email
          else error "too many bcc"

{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.SendEmail.AmazonSES
  ( runSendEmailAmazonSES
  ) where

import Hp.Eff.Log       (LogEffect, log)
import Hp.Eff.SendEmail (SendEmailEffect(..))
import Hp.Email         (Email(..))

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Reader
import Control.Effect.Sum
import Control.Exception.Safe       (try)
import Control.Monad.Trans.Resource (runResourceT)

import qualified Network.AWS               as Aws
import qualified Network.AWS.SES.SendEmail as Aws
import qualified Network.AWS.SES.Types     as Aws


newtype SendEmailCarrierAmazonSES m a
  = SendEmailCarrierAmazonSES
  { unSendEmailCarrierAmazonSES :: ReaderC Aws.Env m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
     ( Carrier sig m
     , Member LogEffect sig
     , MonadIO m
     )
  => Carrier (SendEmailEffect :+: sig) (SendEmailCarrierAmazonSES m) where

  eff ::
       (SendEmailEffect :+: sig)
         (SendEmailCarrierAmazonSES m)
         (SendEmailCarrierAmazonSES m a)
    -> SendEmailCarrierAmazonSES m a
  eff = \case
    L (SendEmail email next) ->
      SendEmailCarrierAmazonSES $ do
        env :: Aws.Env <-
          ask

        doSendEmail env email

        unSendEmailCarrierAmazonSES next

    R other ->
      SendEmailCarrierAmazonSES (eff (R (handleCoercible other)))

-- TODO break email up into max 50 recipients each

doSendEmail ::
     ( Carrier sig m
     , Member LogEffect sig
     , MonadIO m
     )
  => Aws.Env
  -> Email
  -> m ()
doSendEmail env email =
  liftIO (try @_ @Aws.Error (runResourceT (Aws.runAWS env (Aws.send request)))) >>= \case
    Left ex ->
      log (show ex ^. packed)

    Right response ->
      case response ^. Aws.sersResponseStatus of
        200 ->
          pure ()

        _ ->
          log (show response ^. packed)

  where
    request :: Aws.SendEmail
    request =
      case email of
        EmailPersonal email ->
          Aws.sendEmail
            (email ^. #from)
            (Aws.destination
              & Aws.dToAddresses .~ [email ^. #to])
            (Aws.message
              (Aws.content (email ^. #subject))
              (Aws.body
                & Aws.bText .~ Just (Aws.content (email ^. #body))))

        EmailTransactional email ->
          Aws.sendEmail
            (email ^. #from)
            (Aws.destination
              & Aws.dBCCAddresses .~ email ^. #bcc)
            (Aws.message
              (Aws.content (email ^. #subject))
              (Aws.body
                & Aws.bText .~ Just (Aws.content (email ^. #body))))

runSendEmailAmazonSES ::
     Aws.Env
  -> SendEmailCarrierAmazonSES m a
  -> m a
runSendEmailAmazonSES env =
  runReader env . unSendEmailCarrierAmazonSES

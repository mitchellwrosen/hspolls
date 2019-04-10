{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.SendEmail.AmazonSES
  ( runSendEmailAmazonSES
  ) where

import Hp.Eff.SendEmail (SendEmailEffect(..))
import Hp.Email         (Email(..))

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Reader
import Control.Effect.Sum
import Control.Monad.Trans.Resource (runResourceT)

import qualified Network.AWS               as AWS
import qualified Network.AWS.SES.SendEmail as AWS
import qualified Network.AWS.SES.Types     as AWS


newtype SendEmailCarrierAmazonSES m a
  = SendEmailCarrierAmazonSES
  { unSendEmailCarrierAmazonSES :: ReaderC AWS.Env m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
     ( Carrier sig m
     , MonadIO m
     )
  => Carrier (SendEmailEffect :+: sig) (SendEmailCarrierAmazonSES m) where

  eff ::
       (SendEmailEffect :+: sig)
         (SendEmailCarrierAmazonSES m)
         (SendEmailCarrierAmazonSES m a)
    -> SendEmailCarrierAmazonSES m a
  eff = \case
    L (GetMaxEmailRecipients next) ->
      next 50

    L (SendEmail email next) ->
      SendEmailCarrierAmazonSES $ do
        env :: AWS.Env <-
          ask

        doSendEmail env email

        unSendEmailCarrierAmazonSES next

    R other ->
      SendEmailCarrierAmazonSES (eff (R (handleCoercible other)))

doSendEmail ::
     ( Carrier sig m
     , MonadIO m
     )
  => AWS.Env
  -> Email
  -> m ()
doSendEmail env email = do
  _response :: AWS.SendEmailResponse <-
    liftIO (runResourceT (AWS.runAWS env (AWS.send request)))

  -- TODO handle email response
  -- TODO catch IO exceptions

  pure ()

  where
    request :: AWS.SendEmail
    request =
      case email of
        EmailPersonal _ ->
          undefined

        EmailTransactional email ->
          AWS.sendEmail
            "mitchellwrosen@gmail.com"
            (AWS.destination
              & AWS.dBCCAddresses .~ email ^. #bcc)
            (AWS.message
              (AWS.content "Subject!")
              (AWS.body
                & AWS.bText .~ Just (AWS.content "Plaintext email")
                & AWS.bHTML .~ Just (AWS.content "<html><p>HTML email</p></html>")))

runSendEmailAmazonSES ::
     AWS.Env
  -> SendEmailCarrierAmazonSES m a
  -> m a
runSendEmailAmazonSES env =
  runReader env . unSendEmailCarrierAmazonSES

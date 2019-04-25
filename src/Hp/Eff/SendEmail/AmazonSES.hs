module Hp.Eff.SendEmail.AmazonSES
  ( runSendEmailAmazonSES
  ) where

import Hp.Eff.Log       (LogEffect, log)
import Hp.Eff.SendEmail (SendEmailEffect(..))
import Hp.Email         (Email(..))

import Control.Effect
import Control.Effect.Interpret
import Control.Exception.Safe       (try)
import Control.Monad.Trans.Resource (runResourceT)

import qualified Data.List                 as List
import qualified Network.AWS               as Aws
import qualified Network.AWS.SES.SendEmail as Aws
import qualified Network.AWS.SES.Types     as Aws


runSendEmailAmazonSES ::
     ( Carrier sig m
     , Member LogEffect sig
     , MonadIO m
     )
  => Aws.Env
  -> InterpretC SendEmailEffect m a
  -> m a
runSendEmailAmazonSES env =
  runInterpret $ \case
    SendEmail email next -> do
      doSendEmail env email
      next

doSendEmail ::
     ( Carrier sig m
     , Member LogEffect sig
     , MonadIO m
     )
  => Aws.Env
  -> Email
  -> m ()
doSendEmail env email =
  case email of
    EmailPersonal{} ->
      doSendEmail_ env email

    -- 50 recipients at a time
    EmailTransactional email ->
      (`fix` email) $ \loop email ->
        case List.splitAt 50 (email ^. #bcc) of
          (_, []) ->
            doSendEmail_ env (EmailTransactional email)

          (xs, ys) -> do
            doSendEmail_ env (EmailTransactional (email & #bcc .~ xs))
            loop (email & #bcc .~ ys)

-- Precondition: email has <= 50 recipients
doSendEmail_ ::
     ( Carrier sig m
     , Member LogEffect sig
     , MonadIO m
     )
  => Aws.Env
  -> Email
  -> m ()
doSendEmail_ env email =
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

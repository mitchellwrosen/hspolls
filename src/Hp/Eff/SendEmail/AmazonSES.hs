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

import qualified Network.AWS               as Aws
import qualified Network.AWS.SES.SendEmail as Aws
import qualified Network.AWS.SES.Types     as Aws


-- TODO break email up into max 50 recipients each

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
      let
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

      liftIO (try @_ @Aws.Error (runResourceT (Aws.runAWS env (Aws.send request)))) >>= \case
        Left ex -> do
          log (show ex ^. packed)
          next

        Right response ->
          case response ^. Aws.sersResponseStatus of
            200 ->
              next

            _ -> do
              log (show response ^. packed)
              next

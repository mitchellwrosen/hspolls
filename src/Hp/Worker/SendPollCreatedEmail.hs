module Hp.Worker.SendPollCreatedEmail
  ( sendPollCreatedEmailWorker
  ) where

import Hp.Eff.Await         (AwaitEffect, await)
import Hp.Eff.PersistUser   (PersistUserEffect,
                             getUserEmailsSubscribedToPollCreatedEvents)
import Hp.Eff.Yield         (YieldEffect, yield)
import Hp.Email             (Email(..), TransactionalEmail(..))
import Hp.Event.PollCreated (PollCreatedEvent)

import Control.Effect


sendPollCreatedEmailWorker ::
     ( Carrier sig m
     , Member (AwaitEffect PollCreatedEvent) sig
     , Member PersistUserEffect sig
     , Member (YieldEffect Email) sig
     )
  => m void
sendPollCreatedEmailWorker =
  forever $ do
    event :: PollCreatedEvent <-
      await

    emailAddresses :: Vector Text <-
      getUserEmailsSubscribedToPollCreatedEvents

    case handlePollCreatedEvent event emailAddresses of
      Nothing ->
        pure ()

      Just email ->
        yield email

handlePollCreatedEvent ::
     PollCreatedEvent
  -> Vector Text
  -> Maybe Email
handlePollCreatedEvent event addresses = do
  guard (not (null addresses))

  pure $ EmailTransactional TransactionalEmail
    { bcc = toListOf folded addresses
    , body =
        event ^. #poll . #key . Prelude.to show . packed <> " created"
    , from = "mitchellwrosen@gmail.com"
    , subject = "Poll created"
    }

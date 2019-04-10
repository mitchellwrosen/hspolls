-- | Handle events by writing them to a broadcast TChan.
-- TODO newtype TChan to ensure it was made with newBroadcastTChan

{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.Event.Chan
  ( EventCarrierChan
  , runEventChan
  ) where

import Hp.Eff.Event (EventEffect(..))

import Control.Concurrent.STM
import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Reader
import Control.Effect.Sum


newtype EventCarrierChan event m a
  = EventCarrierChan { unEventCarrierChan :: ReaderC (TChan event) m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
     ( Carrier sig m
     , MonadIO m
     )
  => Carrier (EventEffect event :+: sig) (EventCarrierChan event m) where

  eff ::
       (EventEffect event :+: sig) (EventCarrierChan event m) (EventCarrierChan event m a)
    -> EventCarrierChan event m a
  eff = \case
    L (EmitEvent event next) -> do
      EventCarrierChan $ do
        chan :: TChan event <-
          ask
        liftIO (atomically (writeTChan chan event))
      next

    R other ->
      EventCarrierChan (eff (R (handleCoercible other)))

runEventChan ::
     TChan event
  -> EventCarrierChan event m a
  -> m a
runEventChan chan =
  runReader chan . unEventCarrierChan

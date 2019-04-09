{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.Event.Print
  ( EventCarrierPrint
  , runEventPrint
  ) where

import Hp.Eff.Event (EventEffect(..))

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Sum


newtype EventCarrierPrint event m a
  = EventCarrierPrint { unEventCarrierPrint :: m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
     ( Carrier sig m
     , MonadIO m
     , Show event
     )
  => Carrier (EventEffect event :+: sig) (EventCarrierPrint event m) where

  eff ::
       (EventEffect event :+: sig) (EventCarrierPrint event m) (EventCarrierPrint event m a)
    -> EventCarrierPrint event m a
  eff = \case
    L (EmitEvent event next) -> do
      EventCarrierPrint (liftIO (print event))
      next

    R other ->
      EventCarrierPrint (eff (handleCoercible other))

runEventPrint ::
     ∀ event a m.
     EventCarrierPrint event m a
  -> m a
runEventPrint =
  unEventCarrierPrint

{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.Await.Chan
  ( AwaitCarrierChan
  , runAwaitChan
  ) where

import Hp.Eff.Await (AwaitEffect(..))

import Control.Concurrent.STM
import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Reader
import Control.Effect.Sum


newtype AwaitCarrierChan value m a
  = AwaitCarrierChan { unAwaitCarrierChan :: ReaderC (TChan value) m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
     ( Carrier sig m
     , MonadIO m
     )
  => Carrier (AwaitEffect value :+: sig) (AwaitCarrierChan value m) where

  eff ::
       (AwaitEffect value :+: sig) (AwaitCarrierChan value m) (AwaitCarrierChan value m a)
    -> AwaitCarrierChan value m a
  eff = \case
    L (Await next) ->
      AwaitCarrierChan $ do
        chan :: TChan value <-
          ask

        liftIO (atomically (readTChan chan)) >>=
          unAwaitCarrierChan . next

    R other ->
      AwaitCarrierChan (eff (R (handleCoercible other)))

runAwaitChan ::
     TChan value
  -> AwaitCarrierChan value m a
  -> m a
runAwaitChan chan =
  runReader chan . unAwaitCarrierChan

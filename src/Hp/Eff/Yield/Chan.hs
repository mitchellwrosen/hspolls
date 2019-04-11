-- | Handle yields by writing them to a broadcast TChan.

{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.Yield.Chan
  ( YieldCarrierChan
  , runYieldChan
  ) where

import Hp.Eff.Yield (YieldEffect(..))

import Control.Concurrent.STM
import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Reader
import Control.Effect.Sum


newtype YieldCarrierChan value m a
  = YieldCarrierChan { unYieldCarrierChan :: ReaderC (TChan value) m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
     ( Carrier sig m
     , MonadIO m
     )
  => Carrier (YieldEffect value :+: sig) (YieldCarrierChan value m) where

  eff ::
       (YieldEffect value :+: sig) (YieldCarrierChan value m) (YieldCarrierChan value m a)
    -> YieldCarrierChan value m a
  eff = \case
    L (Yield value next) -> do
      YieldCarrierChan $ do
        chan :: TChan value <-
          ask
        liftIO (atomically (writeTChan chan value))
      next

    R other ->
      YieldCarrierChan (eff (R (handleCoercible other)))

runYieldChan ::
     TChan value
  -> YieldCarrierChan value m a
  -> m a
runYieldChan chan =
  runReader chan . unYieldCarrierChan

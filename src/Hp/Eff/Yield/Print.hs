{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.Yield.Print
  ( YieldCarrierPrint
  , runYieldPrint
  ) where

import Hp.Eff.Yield (YieldEffect(..))

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Sum


newtype YieldCarrierPrint value m a
  = YieldCarrierPrint { unYieldCarrierPrint :: m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
     ( Carrier sig m
     , MonadIO m
     , Show value
     )
  => Carrier (YieldEffect value :+: sig) (YieldCarrierPrint value m) where

  eff ::
       (YieldEffect value :+: sig) (YieldCarrierPrint value m) (YieldCarrierPrint value m a)
    -> YieldCarrierPrint value m a
  eff = \case
    L (Yield value next) -> do
      YieldCarrierPrint (liftIO (print value))
      next

    R other ->
      YieldCarrierPrint (eff (handleCoercible other))

runYieldPrint ::
     ∀ value a m.
     YieldCarrierPrint value m a
  -> m a
runYieldPrint =
  unYieldCarrierPrint

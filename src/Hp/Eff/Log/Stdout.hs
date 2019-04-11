-- | Log carrier that synchronously logs to stdout.

{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.Log.Stdout
  ( runLogStdout
  ) where

import Hp.Eff.Log (LogEffect(..))

import Control.Effect.Carrier
import Control.Effect.Sum
import Say (say)


newtype LogCarrierStdout m a
  = LogCarrierStdout { unLogCarrierStdout :: m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
     ( Carrier sig m
     , MonadIO m
     )
  => Carrier (LogEffect :+: sig) (LogCarrierStdout m) where

  eff ::
       (LogEffect :+: sig) (LogCarrierStdout m) (LogCarrierStdout m a)
    -> LogCarrierStdout m a
  eff = \case
    L (Log message next) -> do
      say message
      next

    R other ->
      LogCarrierStdout (eff (handleCoercible other))

runLogStdout ::
     LogCarrierStdout m a
  -> m a
runLogStdout =
  unLogCarrierStdout

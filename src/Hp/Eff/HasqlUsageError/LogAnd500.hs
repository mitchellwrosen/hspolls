-- | Handle hasql usage errors that bubble all the way up to the top of the
-- application by logging them and returning a 500.

{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.HasqlUsageError.LogAnd500
  ( runHasqlUsageErrorLogAnd500
  ) where

import Hp.Eff.Fail (Fail'(..))
import Hp.Eff.Log  (LogEffect, log)

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Error
import Control.Effect.Sum

import qualified Hasql.Pool as Hasql (UsageError)
import qualified Servant    (ServerError, err500)


newtype HasqlUsageErrorCarrierLogAnd500 m a
  = HasqlUsageErrorCarrierLogAnd500
  { unHasqlUsageErrorCarrierLogAnd500 :: m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
     ( Carrier sig m
     , Member (Error Servant.ServerError) sig
     , Member LogEffect sig
     )
  => Carrier
       (Fail' Hasql.UsageError :+: sig)
       (HasqlUsageErrorCarrierLogAnd500 m) where

  eff ::
       (Fail' Hasql.UsageError :+: sig)
         (HasqlUsageErrorCarrierLogAnd500 m)
         (HasqlUsageErrorCarrierLogAnd500 m a)
    -> HasqlUsageErrorCarrierLogAnd500 m a
  eff = \case
    L (Fail' err) ->
      HasqlUsageErrorCarrierLogAnd500 $ do
        log (show err ^. packed)
        throwError Servant.err500

    R other ->
      HasqlUsageErrorCarrierLogAnd500 (eff (handleCoercible other))

runHasqlUsageErrorLogAnd500 ::
     HasqlUsageErrorCarrierLogAnd500 m a
  -> m a
runHasqlUsageErrorLogAnd500 =
  unHasqlUsageErrorCarrierLogAnd500

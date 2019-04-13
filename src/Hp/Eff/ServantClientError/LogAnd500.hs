-- | Handle servant client errors that bubble all the way up to the top of the
-- application by logging them and returning a 500.

{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.ServantClientError.LogAnd500
  ( runServantClientErrorLogAnd500
  ) where

import Hp.Eff.Fail (Fail'(..))
import Hp.Eff.Log  (LogEffect, log)

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Error
import Control.Effect.Sum

import qualified Servant        (ServerError, err500)
import qualified Servant.Client as Servant (ClientError)


newtype ServantClientErrorCarrierLogAnd500 m a
  = ServantClientErrorCarrierLogAnd500
  { unServantClientErrorCarrierLogAnd500 :: m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
     ( Carrier sig m
     , Member (Error Servant.ServerError) sig
     , Member LogEffect sig
     )
  => Carrier
       (Fail' Servant.ClientError :+: sig)
       (ServantClientErrorCarrierLogAnd500 m) where

  eff ::
       (Fail' Servant.ClientError :+: sig)
         (ServantClientErrorCarrierLogAnd500 m)
         (ServantClientErrorCarrierLogAnd500 m a)
    -> ServantClientErrorCarrierLogAnd500 m a
  eff = \case
    L (Fail' err) ->
      ServantClientErrorCarrierLogAnd500 $ do
        log (show err ^. packed)
        throwError Servant.err500

    R other ->
      ServantClientErrorCarrierLogAnd500 (eff (handleCoercible other))

runServantClientErrorLogAnd500 ::
     ServantClientErrorCarrierLogAnd500 m a
  -> m a
runServantClientErrorLogAnd500 =
  unServantClientErrorCarrierLogAnd500

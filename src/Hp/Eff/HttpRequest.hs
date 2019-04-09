{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.HttpRequest
  ( HttpRequestEffect(..)
  , httpRequest
  , fromServantClient
  ) where

import Hp.Eff.FirstOrder (FirstOrderEffect(..))

import Control.Effect
import Control.Effect.Carrier
import Control.Exception      (toException)
import Control.Monad.Free     (Free(..))

import qualified Servant.Client.Core as Servant (BaseUrl, Request, Response)
import qualified Servant.Client.Free as Servant


-- TODO more accurate exception than SomeException
data HttpRequestEffect (m :: Type -> Type) (k :: Type) where
  HttpRequest ::
       Servant.BaseUrl
    -> Servant.Request
    -> (Either SomeException Servant.Response -> k)
    -> HttpRequestEffect m k

  deriving stock (Functor)
  deriving (Effect, HFunctor)
       via (FirstOrderEffect HttpRequestEffect)


httpRequest ::
     ( Carrier sig m
     , Member HttpRequestEffect sig
     )
  => Servant.BaseUrl
  -> Servant.Request
  -> m (Either SomeException Servant.Response)
httpRequest baseUrl request =
  send (HttpRequest baseUrl request pure)

-- | Helper function for translating servant-generated client calls into the
-- HttpRequestEffect effect.
--
-- Assumes (unsafely) that servant calls are *always* structured as containing a
-- request node at the top (Free RunRequest) followed by either a success node
-- (Pure) or a failure node (Free Throw).
fromServantClient ::
     ( Carrier sig m
     , Member HttpRequestEffect sig
     )
  => Servant.BaseUrl
  -> Free Servant.ClientF a
  -> m (Either SomeException a)
fromServantClient baseUrl = \case
  Free (Servant.RunRequest request next) ->
    httpRequest baseUrl request >>= \case
      Left ex ->
        pure (Left ex)

      Right response ->
        case next response of
          Pure token ->
            pure (Right token)

          Free (Servant.Throw err) ->
            pure (Left (toException err))

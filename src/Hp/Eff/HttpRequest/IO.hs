-- | The IO carrier for the http request effect; i.e. the one that actually
-- performs requests.

{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.HttpRequest.IO
  ( runHttpRequestIO
  , HttpConnectionError(..)
  ) where

import Hp.Eff.HttpRequest (HttpRequestEffect(..))
import Hp.Eff.Throw       (ThrowEffect, throw)

import Control.Effect
import Control.Effect.Interpret
import Control.Exception.Safe   (tryAny)

import qualified Network.HTTP.Client                as Http
import qualified Servant.Client                     as Servant
import qualified Servant.Client.Core                as Servant (Request)
import qualified Servant.Client.Internal.HttpClient as Servant (performRequest)


newtype HttpConnectionError
  = HttpConnectionError SomeException
  deriving stock (Show)

-- | Run HTTP requests in IO using the provided HTTP manager. Truly unexpected
-- client errors (due to decoding failures, etc) are thrown, but negative
-- responses from the server are not.
runHttpRequestIO ::
     ( Carrier sig m
     , Member (ThrowEffect HttpConnectionError) sig
     , Member (ThrowEffect Servant.ClientError) sig
     , MonadIO m
     )
  => Http.Manager
  -> InterpretC HttpRequestEffect m a
  -> m a
runHttpRequestIO manager =
  runInterpret $ \case
    HttpRequest baseUrl request next ->
      doHttpRequest manager baseUrl request >>= next

doHttpRequest ::
     ( Carrier sig m
     , Member (ThrowEffect HttpConnectionError) sig
     , Member (ThrowEffect Servant.ClientError) sig
     , MonadIO m
     )
  => Http.Manager
  -> Servant.BaseUrl
  -> Servant.Request
  -> m (Either Servant.Response Servant.Response)
doHttpRequest manager baseUrl request =
  liftIO (tryAny doRequest) >>= \case
    Left ex ->
      throw (HttpConnectionError ex)

    Right (Left (Servant.FailureResponse _ response)) ->
      pure (Left response)

    Right (Left clientError) ->
      throw clientError

    Right (Right response) ->
      pure (Right response)

  where
    doRequest :: IO (Either Servant.ClientError Servant.Response)
    doRequest =
      Servant.runClientM
        (Servant.performRequest request)
        Servant.ClientEnv
          { Servant.manager = manager
          , Servant.baseUrl = baseUrl
          , Servant.cookieJar = Nothing
          }

-- | The IO carrier for the http request effect; i.e. the one that actually
-- performs requests.

{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.HttpRequest.IO
  ( runHttpRequestIO
  ) where

import Hp.Eff.HttpRequest (HttpRequestEffect(..))

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Reader
import Control.Effect.Sum
import Control.Exception.Safe (toException, tryAny)

import qualified Network.HTTP.Client                as Http
import qualified Servant.Client                     as Servant
import qualified Servant.Client.Core                as Servant (Request)
import qualified Servant.Client.Internal.HttpClient as Servant (performRequest)


newtype HttpRequestCarrierIO m a
  = HttpRequestCarrierIO { unHttpRequestCarrierIO :: ReaderC Http.Manager m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
     ( Carrier sig m
     , MonadIO m
     )
  => Carrier (HttpRequestEffect :+: sig) (HttpRequestCarrierIO m) where

  eff ::
       (HttpRequestEffect :+: sig) (HttpRequestCarrierIO m) (HttpRequestCarrierIO m a)
    -> HttpRequestCarrierIO m a
  eff = \case
    L (HttpRequest baseUrl request next) -> do
      HttpRequestCarrierIO $ do
        manager :: Http.Manager <-
          ask

        doHttpRequestIO manager baseUrl request >>=
          unHttpRequestCarrierIO . next

    R other ->
      HttpRequestCarrierIO (eff (R (handleCoercible other)))

doHttpRequestIO ::
     MonadIO m
  => Http.Manager
  -> Servant.BaseUrl
  -> Servant.Request
  -> m (Either SomeException Servant.Response)
doHttpRequestIO manager baseUrl request =
  liftIO (fromResult <$> tryAny doRequest)

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

    fromResult ::
         Either SomeException (Either Servant.ClientError Servant.Response)
      -> Either SomeException Servant.Response
    fromResult = \case
      Left ex ->
        Left ex

      Right (Left err) ->
        Left (toException err)

      Right (Right response) ->
        Right response

runHttpRequestIO ::
     Http.Manager
  -> HttpRequestCarrierIO m a
  -> m a
runHttpRequestIO manager =
  runReader manager . unHttpRequestCarrierIO

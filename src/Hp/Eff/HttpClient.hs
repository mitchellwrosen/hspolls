{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.HttpClient
  ( HttpClient
  , httpRequest
  , fromServantClient
    -- * Carriers
  , HttpManagerC
  , runHttpManager
  ) where

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Reader
import Control.Effect.Sum
import Control.Exception      (toException)
import Control.Exception.Safe (tryAny)
import Control.Monad.Free     (Free(..))
import Data.Generics.Product  (HasType, typed)

import qualified Network.HTTP.Client                as Http
import qualified Servant.Client.Core                as Servant (BaseUrl,
                                                                ClientError,
                                                                Request,
                                                                Response)
import qualified Servant.Client.Free                as Servant
import qualified Servant.Client.Internal.HttpClient as Servant


-- TODO more accurate exception than SomeException
data HttpClient (m :: Type -> Type) (k :: Type) where
  Request ::
       Servant.BaseUrl
    -> Servant.Request
    -> (Either SomeException Servant.Response -> k)
    -> HttpClient m k

  deriving stock (Functor)

-- TODO copy over FirstOrderEffect newtype for deriving via
instance HFunctor HttpClient where
instance Effect HttpClient where


httpRequest ::
     ( Carrier sig m
     , Member HttpClient sig
     )
  => Servant.BaseUrl
  -> Servant.Request
  -> m (Either SomeException Servant.Response)
httpRequest baseUrl request =
  send (Request baseUrl request pure)

-- | Helper function for translating servant-generated client calls into the
-- HttpClient effect.
--
-- Assumes (unsafely) that servant calls are *always* structured as containing a
-- request node at the top (Free RunRequest) followed by either a success node
-- (Pure) or a failure node (Free Throw).
fromServantClient ::
     ( Carrier sig m
     , Member HttpClient sig
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


--------------------------------------------------------------------------------
-- Carriers
--------------------------------------------------------------------------------

newtype HttpManagerC env m a
  = HttpManagerC (m a)
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
     ( Carrier sig m
     , HasType Http.Manager env
     , Member (Reader env) sig
     , MonadIO m
     )
  => Carrier (HttpClient :+: sig) (HttpManagerC env m) where

  eff ::
       (HttpClient :+: sig) (HttpManagerC env m) (HttpManagerC env m a)
    -> HttpManagerC env m a
  eff = \case
    L (Request baseUrl request next) -> do
      HttpManagerC $ do
        manager :: Http.Manager <-
          asks @env (^. typed)

        let
          doRequest :: IO (Either Servant.ClientError Servant.Response)
          doRequest =
            Servant.runClientM
              (Servant.performRequest request)
              Servant.ClientEnv
                { Servant.manager = manager
                , Servant.baseUrl = baseUrl
                , Servant.cookieJar = Nothing
                }

        liftIO (tryAny doRequest) >>= \case
          Left ex ->
            runHttpManager (next (Left ex))

          Right (Left err) ->
            runHttpManager (next (Left (toException err)))

          Right (Right response) ->
            runHttpManager (next (Right response))

    R other ->
      HttpManagerC (eff (handleCoercible other))

runHttpManager ::
     HttpManagerC env m a
  -> m a
runHttpManager (HttpManagerC m) =
  m

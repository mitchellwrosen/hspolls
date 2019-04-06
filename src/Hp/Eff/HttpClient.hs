{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.HttpClient
  ( HttpClient
  , httpRequest
    -- * Carriers
  , HttpManagerC
  , runHttpManager
  ) where

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Error
import Control.Effect.Reader
import Control.Effect.Sum
import Data.Generics.Product  (HasType, typed)

import qualified Network.HTTP.Client                as Http
import qualified Servant.Client.Core                as Servant (BaseUrl,
                                                                ClientError,
                                                                Request,
                                                                Response)
import qualified Servant.Client.Internal.HttpClient as Servant


data HttpClient (m :: Type -> Type) (k :: Type) where
  HttpRequest ::
       Servant.BaseUrl
    -> Servant.Request
    -> (Servant.Response -> k)
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
  -> m Servant.Response
httpRequest baseUrl request =
  send (HttpRequest baseUrl request pure)


--------------------------------------------------------------------------------
-- Carriers
--------------------------------------------------------------------------------

newtype HttpManagerC env m a
  = HttpManagerC (m a)
  deriving newtype (Applicative, Functor, Monad)

instance
     ( Carrier sig m
     , HasType Http.Manager env
     , Member (Error Servant.ClientError) sig
     , Member (Reader env) sig
     , MonadIO m
     )
  => Carrier (HttpClient :+: sig) (HttpManagerC env m) where

  eff ::
       (HttpClient :+: sig) (HttpManagerC env m) (HttpManagerC env m a)
    -> HttpManagerC env m a
  eff = \case
    L (HttpRequest baseUrl request next) -> do
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

        -- TODO what about IO exceptions?
        liftIO doRequest >>= \case
          Left err ->
            throwError err

          Right response ->
            runHttpManager (next response)

    R other ->
      HttpManagerC (eff (handleCoercible other))

runHttpManager ::
     HttpManagerC env m a
  -> m a
runHttpManager (HttpManagerC m) =
  m

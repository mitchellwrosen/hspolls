module Hp.GitHub
  ( clientId
    -- * API
  , postLoginOauthAccessToken
  ) where

import Hp.Eff.HttpClient     (HttpClient, httpRequest)
import Hp.GitHub.AccessToken (AccessToken)
import Hp.GitHub.API         (API)

import qualified Hp.GitHub.API as API

import Control.Effect
import Control.Effect.Error (throwError)
import Control.Monad.Free   (foldFree)

import qualified Servant.Client         as Servant
import qualified Servant.Client.Free    as Servant
import qualified Servant.Client.Generic as Servant


-- | (Temporary) hspolls-test application client ID
-- TODO don't hard code client id even though it's not a secret
clientId :: Text
clientId =
  "0708940f1632f7a953e8"

baseUrl :: Servant.BaseUrl
baseUrl =
  Servant.BaseUrl
    { Servant.baseUrlScheme = Servant.Http -- TODO https
    , Servant.baseUrlHost = "github.com"
    , Servant.baseUrlPort = 80
    , Servant.baseUrlPath = ""
    }


--------------------------------------------------------------------------------
-- Servant-generated client (not very nice to use)
--------------------------------------------------------------------------------

servantClient ::
  forall m sig.
     ( Carrier sig m
     , Member (Error Servant.ClientError) sig
     , Member HttpClient sig
     )
  => API (Servant.AsClientT m)
servantClient =
  Servant.genericClientHoist (foldFree phi)

  where
    phi :: forall x. Servant.ClientF x -> m x
    phi = \case
      Servant.RunRequest request next ->
        next <$> httpRequest baseUrl request

      Servant.Throw err ->
        throwError err


--------------------------------------------------------------------------------
-- Cleaned up client API
--------------------------------------------------------------------------------

postLoginOauthAccessToken ::
     ( Carrier sig m
     , Member (Error Servant.ClientError) sig
     , Member HttpClient sig
     )
  => Text
  -> Text
  -> Text
  -> Maybe Text
  -> Maybe Text
  -> m AccessToken
postLoginOauthAccessToken clientId clientSecret code redirectUri state =
  API.postLoginOauthAccessToken
    servantClient
    clientId
    clientSecret
    code
    redirectUri
    state

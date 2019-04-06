module Hp.GitHub
  ( clientId
    -- * API
  , postLoginOauthAccessToken
  ) where

import Hp.Eff.HttpClient     (HttpClient, fromServantClient, httpRequest)
import Hp.GitHub.AccessToken (AccessToken)
import Hp.GitHub.API         (API)

import qualified Hp.GitHub.API as API

import Control.Effect
import Control.Effect.Error (throwError)
import Control.Monad.Free   (Free(..), foldFree)

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
    { Servant.baseUrlScheme = Servant.Https
    , Servant.baseUrlHost = "github.com"
    , Servant.baseUrlPort = 80
    , Servant.baseUrlPath = ""
    }


--------------------------------------------------------------------------------
-- Internal servant-generated client
--------------------------------------------------------------------------------

servantClient :: API (Servant.AsClientT (Free Servant.ClientF))
servantClient =
  Servant.genericClient


--------------------------------------------------------------------------------
-- Cleaned up client API
--------------------------------------------------------------------------------

postLoginOauthAccessToken ::
     ( Carrier sig m
     , Member HttpClient sig
     )
  => Text
  -> Text
  -> Text
  -> Maybe Text
  -> Maybe Text
  -> m (Either SomeException AccessToken)
postLoginOauthAccessToken clientId clientSecret code redirectUri state =
  fromServantClient
    baseUrl
    (API.postLoginOauthAccessToken
      servantClient
      clientId
      clientSecret
      code
      redirectUri
      state)

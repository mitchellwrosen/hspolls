module Hp.GitHub
  ( gitHubGetUser
  , gitHubPostLoginOauthAccessToken
  ) where

import Hp.Eff.HttpRequest                          (HttpRequestEffect,
                                                    fromServantClient)
import Hp.GitHub.AccessToken                       (GitHubAccessToken)
import Hp.GitHub.API                               (GitHubAPI)
import Hp.GitHub.ClientId                          (GitHubClientId(..))
import Hp.GitHub.ClientSecret                      (GitHubClientSecret)
import Hp.GitHub.Code                              (GitHubCode)
import Hp.GitHub.PostLoginOauthAccessTokenResponse (GitHubPostLoginOauthAccessTokenResponse)
import Hp.GitHub.Response                          (GitHubResponse)
import Hp.GitHub.User                              (GitHubUser)

import qualified Hp.GitHub.API as API

import Control.Effect
import Control.Monad.Free (Free(..))

import qualified Servant.Client         as Servant
import qualified Servant.Client.Free    as Servant
import qualified Servant.Client.Generic as Servant


baseUrl :: Servant.BaseUrl
baseUrl =
  Servant.BaseUrl
    { Servant.baseUrlScheme = Servant.Https
    , Servant.baseUrlHost = "github.com"
    , Servant.baseUrlPort = 443
    , Servant.baseUrlPath = ""
    }

apiBaseUrl :: Servant.BaseUrl
apiBaseUrl =
  Servant.BaseUrl
    { Servant.baseUrlScheme = Servant.Https
    , Servant.baseUrlHost = "api.github.com"
    , Servant.baseUrlPort = 443
    , Servant.baseUrlPath = ""
    }

userAgent :: Text
userAgent =
  "hspolls"


--------------------------------------------------------------------------------
-- Internal servant-generated client
--------------------------------------------------------------------------------

servantClient :: GitHubAPI (Servant.AsClientT (Free Servant.ClientF))
servantClient =
  Servant.genericClient


--------------------------------------------------------------------------------
-- Cleaned up client API
--------------------------------------------------------------------------------

gitHubGetUser ::
     ( Carrier sig m
     , Member HttpRequestEffect sig
     )
  => GitHubAccessToken
  -> m (Either SomeException GitHubUser)
gitHubGetUser accessToken =
  fromServantClient
    apiBaseUrl
    (API.gitHubGetUser
      servantClient
      userAgent
      accessToken)

gitHubPostLoginOauthAccessToken ::
     ( Carrier sig m
     , Member HttpRequestEffect sig
     )
  => GitHubClientId
  -> GitHubClientSecret
  -> GitHubCode
  -> Maybe Text
  -> Maybe Text
  -> m (Either SomeException (GitHubResponse GitHubPostLoginOauthAccessTokenResponse))
gitHubPostLoginOauthAccessToken clientId clientSecret code redirectUri state =
  fromServantClient
    baseUrl
    (API.gitHubPostLoginOauthAccessToken
      servantClient
      clientId
      clientSecret
      code
      redirectUri
      state)

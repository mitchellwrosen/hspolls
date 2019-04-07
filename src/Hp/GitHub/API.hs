module Hp.GitHub.API where

import Hp.GitHub.AccessToken                       (GitHubAccessToken)
import Hp.GitHub.ClientId                          (GitHubClientId)
import Hp.GitHub.ClientSecret                      (GitHubClientSecret)
import Hp.GitHub.Code                              (GitHubCode)
import Hp.GitHub.PostLoginOauthAccessTokenResponse (GitHubPostLoginOauthAccessTokenResponse)
import Hp.GitHub.Response                          (GitHubResponse)
import Hp.GitHub.User                              (GitHubUser)

import Servant.API
import Servant.API.Generic


data GitHubAPI route
  = GitHubAPI
  { -- | https://developer.github.com/v3/users/#get-the-authenticated-user
    gitHubGetUser
      :: route
      :- "user"
      :> Header' '[Required, Strict] "User-Agent" Text
      :> Header' '[Required, Strict] "Authorization" GitHubAccessToken
      :> Get '[JSON] GitHubUser

    -- | https://developer.github.com/apps/building-oauth-apps/authorizing-oauth-apps/#2-users-are-redirected-back-to-your-site-by-github
  , gitHubPostLoginOauthAccessToken
      :: route
      :- "login"
      :> "oauth"
      :> "access_token"
      :> QueryParam' '[Required, Strict] "client_id" GitHubClientId
      :> QueryParam' '[Required, Strict] "client_secret" GitHubClientSecret
      :> QueryParam' '[Required, Strict] "code" GitHubCode
      :> QueryParam' '[Optional, Strict] "redirect_uri" Text
      :> QueryParam' '[Optional, Strict] "state" Text
      :> Get '[JSON] (GitHubResponse GitHubPostLoginOauthAccessTokenResponse)
  } deriving stock (Generic)

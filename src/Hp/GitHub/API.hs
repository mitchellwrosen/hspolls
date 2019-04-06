module Hp.GitHub.API where

import Hp.GitHub.AccessToken (AccessToken)

import Servant.API
import Servant.API.Generic


data API route
  = API
  { -- | https://developer.github.com/apps/building-oauth-apps/authorizing-oauth-apps/#2-users-are-redirected-back-to-your-site-by-github
    postLoginOauthAccessToken
      :: route
      :- "login"
      :> "oauth"
      :> "access_token"
      :> QueryParam' '[Required, Strict] "client_id" Text
      :> QueryParam' '[Required, Strict] "client_secret" Text
      :> QueryParam' '[Required, Strict] "code" Text
      :> QueryParam' '[Optional, Strict] "redirect_uri" Text
      :> QueryParam' '[Optional, Strict] "state" Text
      :> Get '[JSON] AccessToken
  } deriving stock (Generic)

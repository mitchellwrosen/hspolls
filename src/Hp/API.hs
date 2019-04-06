module Hp.API where

import Hp.Form

import Servant
import Servant.API.Generic
import Servant.HTML.Blaze

import qualified Text.Blaze.Html as Blaze


data API route
  = API
  { getLoginRoute
      :: route
      :- "login"
      :> Get '[HTML] Blaze.Html

    -- Callback URL used for GitHub OAuth
  , getLoginGitHubRoute
      :: route
      :- "login"
      :> "github"
      :> QueryParam' '[Required, Strict] "code" Text
      -- TODO newtype the "code"
      -- TODO required "state" query param
      -- TODO just returning html for now, but should redirect
      :> Get '[HTML] Blaze.Html

  , postPollRoute
      :: route
      :- "poll"
      :> ReqBody '[JSON] Form
      :> Post '[JSON] NoContent
  } deriving stock (Generic)

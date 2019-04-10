module Hp.API where

import Hp.Entity                 (Entity)
import Hp.GitHub.Code            (GitHubCode)
import Hp.PollId                 (PollId)
import Hp.RequestBody.AnswerPoll (AnswerPollRequestBody)
import Hp.RequestBody.CreatePoll (CreatePollRequestBody)
import Hp.User                   (User)

import Servant
import Servant.API.Generic
import Servant.Auth        (Auth, Cookie)
import Servant.Auth.Server (SetCookie)
import Servant.HTML.Blaze

import qualified Text.Blaze.Html as Blaze


data API route
  = API
  { answerPollRoute
      :: route
      :- Auth '[Cookie] (Entity User)
      :> "poll"
      :> Capture "Poll ID" PollId
      :> ReqBody '[JSON] AnswerPollRequestBody
      :> Post '[JSON] NoContent

  , createPollRoute
      :: route
      :- "poll"
      :> ReqBody '[JSON] CreatePollRequestBody
      :> Post '[JSON] NoContent

  , getMetricsRoute
      :: route
      :- "metrics"
      :> Get '[PlainText] Text

  , getRootRoute
      :: route
      :- Auth '[Cookie] (Entity User)
      :> Get '[HTML] Blaze.Html

    -- Callback URL used for GitHub OAuth.
  , gitHubOauthCallbackRoute
      :: route
      :- "oauth"
      :> "github"
      :> QueryParam' '[Required, Strict] "code" GitHubCode
      -- TODO required "state" query param
      -- TODO just returning html for now, but should redirect
      :> Verb
           'GET
           302
           '[HTML]
           (Headers
             '[ Header "Location" Text
              , Header "Set-Cookie" SetCookie
              , Header "Set-Cookie" SetCookie
              ]
           NoContent)
  } deriving stock (Generic)

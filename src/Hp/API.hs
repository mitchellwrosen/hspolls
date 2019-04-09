module Hp.API where

import Hp.Event.AnswerPoll (AnswerPollEvent)
import Hp.GitHub.Code      (GitHubCode)
import Hp.Poll             (Poll, PollId)
import Hp.User             (User)
import Hp.UserId           (UserId)

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
      :- "poll"
      :> Capture "Poll ID" PollId
      :> ReqBody '[JSON] AnswerPollEvent
      :> Post '[JSON] NoContent

  , createPollRoute
      :: route
      :- "poll"
      :> ReqBody '[JSON] Poll
      :> Post '[JSON] NoContent

  , getMetricsRoute
      :: route
      :- "metrics"
      :> Get '[PlainText] Text

  , getRootRoute
      :: route
      :- Auth '[Cookie] (User UserId)
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

module Hp.API
  ( API(..)
  ) where

import Hp.Entity.Poll            (PollId)
import Hp.Entity.User            (UserId)
import Hp.GitHub.Code            (GitHubCode)
import Hp.RequestBody.AnswerPoll (AnswerPollRequestBody)
import Hp.RequestBody.CreatePoll (CreatePollRequestBody)
import Hp.RequestBody.Subscribe  (SubscribeRequestBody)
import Hp.ResponseBody.GetPoll   (GetPollResponseBody)
import Hp.UserProfile            (UserProfile)

import Servant
import Servant.API.Generic
import Servant.Auth        (Auth, Cookie)
import Servant.Auth.Server (SetCookie)
import Servant.HTML.Blaze

import qualified Text.Blaze.Html as Blaze


data API route
  = API
  { -- | Answer a poll.
    answerPollRoute
      :: route
      :- Auth '[Cookie] UserId
      :> "poll"
      :> Capture "PollId" PollId
      :> ReqBody '[JSON] AnswerPollRequestBody
      :> Post '[JSON] NoContent

    -- | Create a poll.
  , createPollRoute
      :: route
      :- Auth '[Cookie] UserId
      :> "poll"
      :> ReqBody '[JSON] CreatePollRequestBody
      :> Post '[JSON] PollId

    -- | Get Prometheus metrics.
  , getMetricsRoute
      :: route
      :- "metrics"
      :> Get '[PlainText] Text

  , getPollRoute
      :: route
      :- "poll"
      :> Capture "PollId" PollId
      :> Get '[JSON] GetPollResponseBody

  , getRootRoute
      :: route
      :- Auth '[Cookie] UserId
      :> Get '[HTML] Blaze.Html

  , getUserProfileRoute
      :: route
      :- Auth '[Cookie] UserId
      :> "profile"
      :> Get '[JSON] UserProfile

    -- | Callback URL used for GitHub OAuth.
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

    -- | Adjust subscription settings.
  , subscribeRoute
      :: route
      :- Auth '[Cookie] UserId
      :> "subscribe"
      :> ReqBody '[JSON] SubscribeRequestBody
      :> Post '[JSON] NoContent
  } deriving stock (Generic)

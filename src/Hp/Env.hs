module Hp.Env where

import Hp.GitHub.ClientId     (GitHubClientId)
import Hp.GitHub.ClientSecret (GitHubClientSecret)

import qualified Network.HTTP.Client as Http


data Env
  = Env
  { manager :: Http.Manager
  , gitHubClientId :: GitHubClientId
  , gitHubClientSecret :: GitHubClientSecret
  } deriving stock (Generic)

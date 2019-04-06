module Hp.Env where

import Hp.GitHub.ClientSecret (GitHubClientSecret)

import qualified Network.HTTP.Client as Http


data Env
  = Env
  { manager :: Http.Manager
  , gitHubClientSecret :: GitHubClientSecret
  } deriving stock (Generic)

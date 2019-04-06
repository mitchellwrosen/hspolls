module Hp.Env where

import qualified Hp.GitHub.ClientSecret as Hp.GitHub (ClientSecret)

import qualified Network.HTTP.Client as Http


data Env
  = Env
  { manager :: Http.Manager
  , gitHubClientSecret :: Hp.GitHub.ClientSecret
  } deriving stock (Generic)

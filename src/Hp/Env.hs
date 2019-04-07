module Hp.Env where

import Hp.GitHub.ClientId     (GitHubClientId)
import Hp.GitHub.ClientSecret (GitHubClientSecret)

import qualified Network.HTTP.Client as Http
import qualified Hasql.Pool as HPool


data Env
  = Env
  { manager :: Http.Manager
  , gitHubClientId :: GitHubClientId
  , gitHubClientSecret :: GitHubClientSecret
  , postgresPool :: HPool.Pool
  } deriving stock (Generic)

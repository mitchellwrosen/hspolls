module Hp.Env where

import Hp.GitHub.ClientId     (GitHubClientId)
import Hp.GitHub.ClientSecret (GitHubClientSecret)

import Crypto.JOSE.JWK (JWK)

import qualified Hasql.Pool          as HPool
import qualified Network.HTTP.Client as Http


data Env
  = Env
  { httpManager :: Http.Manager
  , gitHubClientId :: GitHubClientId
  , gitHubClientSecret :: GitHubClientSecret
  , jwk :: JWK
  , postgresPool :: HPool.Pool
  } deriving stock (Generic)

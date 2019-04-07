module Hp.Env where

import Hp.GitHub.ClientId     (GitHubClientId)
import Hp.GitHub.ClientSecret (GitHubClientSecret)

import Servant.Auth.Server (CookieSettings, JWTSettings)

import qualified Hasql.Pool          as HPool
import qualified Network.HTTP.Client as Http


data Env
  = Env
  { cookieSettings :: CookieSettings
  , httpManager :: Http.Manager
  , gitHubClientId :: GitHubClientId
  , gitHubClientSecret :: GitHubClientSecret
  , jwtSettings :: JWTSettings
  , postgresPool :: HPool.Pool
  } deriving stock (Generic)

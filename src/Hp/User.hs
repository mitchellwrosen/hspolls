module Hp.User
  ( User(..)
  ) where

import Hp.GitHub.UserName (GitHubUserName)

import Data.Aeson          (FromJSON, ToJSON)
import Servant.Auth.Server (FromJWT, ToJWT)


-- TODO encrypt user in ToJWT
data User id
  = User
  { id :: id
  , gitHub :: Maybe GitHubUserName
  } deriving stock (Generic, Show)
    deriving anyclass (FromJSON, FromJWT, ToJSON, ToJWT)

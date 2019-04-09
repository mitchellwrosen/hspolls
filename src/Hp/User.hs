module Hp.User
  ( User(..)
  , userEncoder
  ) where

import Hp.GitHub.UserName (GitHubUserName, gitHubUserNameEncoder)

import Data.Aeson          (FromJSON, ToJSON)
import Servant.Auth.Server (FromJWT, ToJWT)

import qualified Hasql.Encoders as Encoder


-- TODO encrypt user in ToJWT
data User id
  = User
  { id :: id
  , gitHub :: Maybe GitHubUserName
  } deriving stock (Generic, Show)
    deriving anyclass (FromJSON, FromJWT, ToJSON, ToJWT)

userEncoder :: Encoder.Params (User ())
userEncoder =
  (^. #gitHub) >$< Encoder.nullableParam gitHubUserNameEncoder

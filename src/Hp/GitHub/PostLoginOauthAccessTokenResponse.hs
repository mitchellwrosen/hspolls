module Hp.GitHub.PostLoginOauthAccessTokenResponse
  ( GitHubPostLoginOauthAccessTokenResponse(..)
  ) where

import Hp.GitHub.AccessToken (GitHubAccessToken)

import Data.Aeson (FromJSON)


data GitHubPostLoginOauthAccessTokenResponse
  = GitHubPostLoginOauthAccessTokenResponse
  { access_token :: GitHubAccessToken
  , scope :: Text
  , token_type :: Text
  } deriving stock (Show, Generic)
    deriving anyclass (FromJSON)

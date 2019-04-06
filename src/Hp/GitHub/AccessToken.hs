module Hp.GitHub.AccessToken where

import Data.Aeson      (FromJSON)
import Web.HttpApiData (ToHttpApiData)


newtype GitHubAccessToken
  = GitHubAccessToken { unGitHubAccessToken :: Text }
  deriving stock (Show)
  deriving newtype (FromJSON, ToHttpApiData)

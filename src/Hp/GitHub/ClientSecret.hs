module Hp.GitHub.ClientSecret where

import Web.HttpApiData


newtype GitHubClientSecret
  = GitHubClientSecret { unGitHubClientSecret :: Text }
  deriving newtype (ToHttpApiData)

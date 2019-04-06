module Hp.GitHub.ClientId where

import Web.HttpApiData


newtype GitHubClientId
  = GitHubClientId { unGitHubClientId :: Text }
  deriving newtype (ToHttpApiData)

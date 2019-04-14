module Hp.GitHub.ClientId
  ( GitHubClientId(..)
  ) where

import Web.HttpApiData


newtype GitHubClientId
  = GitHubClientId { unGitHubClientId :: Text }
  deriving newtype (Show, ToHttpApiData)

module Hp.GitHub.Code where

import Web.HttpApiData


newtype GitHubCode
  = GitHubCode { unGitHubCode :: Text }
  deriving newtype (FromHttpApiData, ToHttpApiData)

module Hp.GitHub.Code
  ( GitHubCode(..)
  ) where

import Web.HttpApiData


newtype GitHubCode
  = GitHubCode { unGitHubCode :: Text }
  deriving newtype (FromHttpApiData, ToHttpApiData)

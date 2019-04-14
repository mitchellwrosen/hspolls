module Hp.GitHub.ClientSecret
  ( GitHubClientSecret(..)
  ) where

import Web.HttpApiData


newtype GitHubClientSecret
  = GitHubClientSecret { unGitHubClientSecret :: Text }
  deriving newtype (ToHttpApiData)

instance Show GitHubClientSecret where
  show :: GitHubClientSecret -> [Char]
  show _ =
    "<GitHubClientSecret>"

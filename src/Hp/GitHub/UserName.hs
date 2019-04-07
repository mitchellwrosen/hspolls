module Hp.GitHub.UserName
  ( GitHubUserName(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)


newtype GitHubUserName
  = GitHubUserName { unGitHubUserName :: Text }
  deriving stock (Show)
  deriving newtype (FromJSON, ToJSON)

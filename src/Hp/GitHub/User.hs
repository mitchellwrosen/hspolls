module Hp.GitHub.User where

import Data.Aeson (FromJSON)


newtype GitHubUser
  = GitHubUser
  { login :: Text
  } deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

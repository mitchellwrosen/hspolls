module Hp.GitHub.User where

import Hp.GitHub.UserName

import Data.Aeson (FromJSON)


newtype GitHubUser
  = GitHubUser
  { login :: GitHubUserName
  } deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

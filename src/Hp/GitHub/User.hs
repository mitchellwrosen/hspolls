module Hp.GitHub.User
  ( GitHubUser(..)
  ) where

import Hp.GitHub.UserName

import Data.Aeson (FromJSON)


data GitHubUser
  = GitHubUser
  { email :: Maybe Text
  , login :: GitHubUserName
  } deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

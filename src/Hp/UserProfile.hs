module Hp.UserProfile
  ( UserProfile(..)
  ) where

import Hp.GitHub.UserName (GitHubUserName)

import Data.Aeson (ToJSON)


data UserProfile
  = UserProfile
  { gitHub :: Maybe GitHubUserName
  , subscribedToPollCreated :: Bool
  } deriving stock (Generic)
    deriving anyclass (ToJSON)

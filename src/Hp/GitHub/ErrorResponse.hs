module Hp.GitHub.ErrorResponse
  ( GitHubErrorResponse(..)
  ) where

import Data.Aeson (FromJSON)


data GitHubErrorResponse
  = GitHubErrorResponse
  { error :: Text
  , error_description :: Text
  , error_uri :: Text
  } deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

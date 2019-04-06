module Hp.GitHub.ErrorResponse where

import Data.Aeson (FromJSON)


data ErrorResponse
  = ErrorResponse
  { error :: Text
  , error_description :: Text
  , error_uri :: Text
  } deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

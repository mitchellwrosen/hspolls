module Hp.GitHub.AccessToken where

import Data.Aeson (FromJSON)


data AccessToken
  = AccessToken
  { access_token :: Text
  , scope :: Text
  , token_type :: Text
  } deriving stock (Generic)
    deriving anyclass (FromJSON)

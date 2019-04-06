module Hp.GitHub.Response where

import Hp.GitHub.ErrorResponse (GitHubErrorResponse)

import Data.Aeson       (FromJSON(..), Value)
import Data.Aeson.Types (Parser)


data GitHubResponse a
  = GitHubResponseError GitHubErrorResponse
  | GitHubResponseSuccess a
  deriving stock (Show)

instance FromJSON a => FromJSON (GitHubResponse a) where
  parseJSON :: Value -> Parser (GitHubResponse a)
  parseJSON value =
    asum
      [ GitHubResponseError <$> parseJSON value
      , GitHubResponseSuccess <$> parseJSON value
      ]

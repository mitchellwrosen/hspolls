module Hp.GitHub.AccessToken where

import Data.Aeson      (FromJSON)
import Web.HttpApiData (ToHttpApiData(..))


newtype GitHubAccessToken
  = GitHubAccessToken { unGitHubAccessToken :: Text }
  deriving stock (Show)
  deriving newtype (FromJSON)

instance ToHttpApiData GitHubAccessToken where
  toQueryParam :: GitHubAccessToken -> Text
  toQueryParam =
    coerce

  toHeader :: GitHubAccessToken -> ByteString
  toHeader (GitHubAccessToken token) =
    "token " <> toHeader token

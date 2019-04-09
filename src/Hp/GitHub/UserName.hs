module Hp.GitHub.UserName
  ( GitHubUserName(..)
  , gitHubUserNameEncoder
  , gitHubUserNameDecoder
  ) where

import Data.Aeson (FromJSON, ToJSON)

import qualified Hasql.Decoders as Decoder
import qualified Hasql.Encoders as Encoder


newtype GitHubUserName
  = GitHubUserName { unGitHubUserName :: Text }
  deriving stock (Show)
  deriving newtype (FromJSON, ToJSON)

gitHubUserNameEncoder :: Encoder.Value GitHubUserName
gitHubUserNameEncoder =
  coerce Encoder.text

gitHubUserNameDecoder :: Decoder.Value GitHubUserName
gitHubUserNameDecoder =
  GitHubUserName <$> Decoder.text

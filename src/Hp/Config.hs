{-# LANGUAGE ApplicativeDo #-}

module Hp.Config
  ( Config(..)
  , readConfigFile
  , prettyPrintConfig
  ) where

import Hp.GitHub.ClientId     (GitHubClientId(..))
import Hp.GitHub.ClientSecret (GitHubClientSecret(..))

import Crypto.JOSE.JWK         (JWK)
import Data.ByteArray.Encoding (Base(..), convertFromBase)
import Data.Validation

import qualified Crypto.JOSE.JWK as JWK
import qualified Data.ByteString as ByteString
import qualified Data.Text.IO    as Text
import qualified Dhall


-- | Config parsed straight from a dhall value, with no additional checks on the
-- values (e.g. the JWT must be a certain length string).
data UnvalidatedConfig
  = UnvalidatedConfig
  { gitHub :: UnvalidatedGitHubConfig
  , jwk :: Text
  , port :: Natural
  } deriving stock (Generic)
    deriving anyclass (Dhall.Interpret)

data UnvalidatedGitHubConfig
  = UnvalidatedGitHubConfig
  { clientId :: Text
  , clientSecret :: Text
  } deriving stock (Generic)
    deriving anyclass (Dhall.Interpret)

data Config
  = Config
  { gitHub :: GitHubConfig
  , jwk :: JWK
  , port :: Natural
  } deriving stock (Generic, Show)

data GitHubConfig
  = GitHubConfig
  { clientId :: GitHubClientId
  , clientSecret :: GitHubClientSecret
  } deriving stock (Generic, Show)

readConfigFile :: FilePath -> IO (Either [Text] Config)
readConfigFile path = do
  unvalidatedConfig :: UnvalidatedConfig <-
    Dhall.detailed (Dhall.input Dhall.auto (path ^. packed))

  pure (toEither (validateConfig unvalidatedConfig))

validateConfig :: UnvalidatedConfig -> Validation [Text] Config
validateConfig config = do
  jwk :: JWK <-
    validateJWK (config ^. #jwk)

  pure Config
    { gitHub =
        GitHubConfig
          { clientId =
              GitHubClientId (config ^. #gitHub . #clientId)
          , clientSecret =
              GitHubClientSecret (config ^. #gitHub . #clientSecret)
          }

    , jwk =
        jwk

      -- TODO validate port is < 2^6
    , port =
        config ^. #port
    }

validateJWK :: Text -> Validation [Text] JWK
validateJWK bytes =
  case convertFromBase Base64 (bytes ^. re utf8) :: Either String ByteString of
    Left err ->
      Failure ["Invalid JWK (expected 256 base64-encoded bytes): " <> err ^. packed]

    Right bytes ->
      case ByteString.length bytes of
        256 ->
          pure (JWK.fromOctets bytes)

        _ ->
          Failure ["Invalid JWK (expected 256 base64-encoded bytes)"]

prettyPrintConfig :: Config -> IO ()
prettyPrintConfig config = do
  prettyPrintGitHubConfig (config ^. #gitHub)
  Text.putStrLn "jwk = <JWK>"
  Text.putStrLn ("port = " <> (config ^. #port . to show . packed))

  where
    prettyPrintGitHubConfig :: GitHubConfig -> IO ()
    prettyPrintGitHubConfig config = do
      Text.putStrLn ("GitHub client id = " <> (config ^. #clientId . to show . packed))
      Text.putStrLn ("GitHub client secret = " <> (config ^. #clientSecret . to show . packed))

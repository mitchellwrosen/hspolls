{-# LANGUAGE ApplicativeDo #-}

module Hp.Config
  ( Config(..)
  , AwsConfig(..)
  , PostgresConfig(..)
  , readConfigFile
  , prettyPrintConfig
  ) where

import Hp.GitHub.ClientId     (GitHubClientId(..))
import Hp.GitHub.ClientSecret (GitHubClientSecret(..))

import Crypto.JOSE.JWK         (JWK)
import Data.ByteArray.Encoding (Base(..), convertFromBase)
import Data.Validation
import Servant.Auth.Server     (CookieSettings(..), IsSecure(..),
                                JWTSettings(..), SameSite(..),
                                defaultJWTSettings, defaultXsrfCookieSettings,
                                generateKey)

import qualified Crypto.JOSE.JWK as JWK
import qualified Data.ByteString as ByteString
import qualified Data.Text.IO    as Text
import qualified Dhall
import qualified Network.AWS     as AWS


-- | Config parsed straight from a dhall value, with no additional checks on the
-- values (e.g. the JWT must be a certain length string).
data UnvalidatedConfig
  = UnvalidatedConfig
  { aws :: UnvalidatedAwsConfig
  , gitHub :: UnvalidatedGitHubConfig
  , port :: Natural
  , postgres :: UnvalidatedPostgresConfig
  , session :: UnvalidatedSessionConfig
  } deriving stock (Generic)
    deriving anyclass (Dhall.Interpret)

data UnvalidatedAwsConfig
  = UnvalidatedAwsConfig
  { accessKeyId :: Text
  , secretAccessKey :: Text
  } deriving stock (Generic)
    deriving anyclass (Dhall.Interpret)

data UnvalidatedGitHubConfig
  = UnvalidatedGitHubConfig
  { clientId :: Text
  , clientSecret :: Text
  } deriving stock (Generic)
    deriving anyclass (Dhall.Interpret)

data UnvalidatedPostgresConfig
  = UnvalidatedPostgresConfig
  { host :: Text
  , port :: Natural
  , user :: Text
  , password :: Text
  , dbName :: Text
  , poolSize :: Natural
  , poolTimeout :: Natural
  } deriving stock (Generic, Show)
    deriving anyclass (Dhall.Interpret)

data UnvalidatedSessionConfig
  = UnvalidatedSessionConfig
  { jwk :: Maybe Text
  , name :: Text
  , secure :: Bool
  , ttl :: Maybe Natural
  , xsrf :: Bool
  } deriving stock (Generic)
    deriving anyclass (Dhall.Interpret)

data Config
  = Config
  { aws :: AwsConfig
  , gitHub :: GitHubConfig
  , port :: Natural
  , postgres :: PostgresConfig
  , session :: SessionConfig
  } deriving stock (Generic)

data AwsConfig
  = AwsConfig
  { accessKeyId :: AWS.AccessKey
  , secretAccessKey :: AWS.SecretKey
  } deriving stock (Generic)

data GitHubConfig
  = GitHubConfig
  { clientId :: GitHubClientId
  , clientSecret :: GitHubClientSecret
  } deriving stock (Generic, Show)

data PostgresConfig
  = PostgresConfig
  { host :: Text
  , port :: Natural
  , user :: Text
  , password :: Text
  , dbName :: Text
  , poolSize :: Natural
  , poolTimeout :: Natural
  } deriving stock (Generic, Show)

data SessionConfig
  = SessionConfig
  { cookie :: CookieSettings
  , jwt :: Either (IO JWTSettings) JWTSettings
  } deriving stock (Generic)

readConfigFile :: FilePath -> IO (Either [Text] Config)
readConfigFile path = do
  unvalidatedConfig :: UnvalidatedConfig <-
    Dhall.detailed (Dhall.input Dhall.auto (path ^. packed))

  pure (toEither (validateConfig unvalidatedConfig))

validateConfig :: UnvalidatedConfig -> Validation [Text] Config
validateConfig config = do
  aws :: AwsConfig <-
    validateAwsConfig (config ^. #aws)

  gitHub :: GitHubConfig <-
    validateGitHubConfig (config ^. #gitHub)

  postgres :: PostgresConfig <-
    validatePostgresConfig (config ^. #postgres)

  session :: SessionConfig <-
    validateSessionConfig (config ^. #session)

  pure Config
    { aws = aws
    , gitHub = gitHub
      -- TODO validate port is < 2^6
    , port = config ^. #port
    , postgres = postgres
    , session = session
    }

validateAwsConfig ::
     UnvalidatedAwsConfig
  -> Validation [Text] AwsConfig
validateAwsConfig config =
  pure AwsConfig
    { accessKeyId = AWS.AccessKey (config ^. #accessKeyId . re utf8)
    , secretAccessKey = AWS.SecretKey (config ^. #secretAccessKey . re utf8)
    }

validateGitHubConfig ::
     UnvalidatedGitHubConfig
  -> Validation [Text] GitHubConfig
validateGitHubConfig config =
  pure GitHubConfig
    { clientId = GitHubClientId (config ^. #clientId)
    , clientSecret = GitHubClientSecret (config ^. #clientSecret)
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

validateSessionConfig ::
     UnvalidatedSessionConfig
  -> Validation [Text] SessionConfig
validateSessionConfig config =
  SessionConfig
    <$> validateCookieSettings
    <*> validateJWTSettings

  where
    validateCookieSettings :: Validation [Text] CookieSettings
    validateCookieSettings =
      pure CookieSettings
        { cookieDomain =
            Nothing

        , cookieExpires =
            Nothing

        , cookieIsSecure =
            if config ^. #secure
              then Secure
              else NotSecure

        , cookieMaxAge =
            fromIntegral <$> (config ^. #ttl)

        , cookiePath =
            Just "/"

        , cookieSameSite =
            SameSiteLax

        , cookieXsrfSetting =
            if config ^. #xsrf
              then Just defaultXsrfCookieSettings
              else Nothing

        , sessionCookieName =
            config ^. #name . re utf8
        }

    validateJWTSettings ::
         Validation [Text] (Either (IO JWTSettings) JWTSettings)
    validateJWTSettings =
      case config ^. #jwk of
        Nothing ->
          pure (Left (defaultJWTSettings <$> generateKey))

        Just bytes -> do
          jwk :: JWK <-
            validateJWK bytes

          pure (Right (defaultJWTSettings jwk))

validatePostgresConfig ::
     UnvalidatedPostgresConfig
  -> Validation [Text] PostgresConfig
validatePostgresConfig config =
  pure PostgresConfig
    { host = config ^. #host
    , port = config ^. #port
    , user = config ^. #user
    , password = config ^. #password
    , dbName = config ^. #dbName
    , poolSize = config ^. #poolSize
    , poolTimeout = config ^. #poolTimeout
    }

prettyPrintConfig :: Config -> IO ()
prettyPrintConfig config = do
  Text.putStrLn "aws_access_key_id = <AccessKey>"
  Text.putStrLn "aws_secret_access_key = <SecretKey>"
  Text.putStrLn $
    "github_client_id = " <> config ^. #gitHub . #clientId . to show . packed
  Text.putStrLn $
    "github_client_secret = " <>
      config ^. #gitHub . #clientSecret . to show . packed
  Text.putStrLn ("port = " <> config ^. #port . to show . packed)
  Text.putStrLn $
    "postgres_db_name = \"" <> config ^. #postgres . #dbName <> "\""
  Text.putStrLn $
    "postgres_host = \"" <> config ^. #postgres . #host <> "\""
  Text.putStrLn $
    "postgres_password = \"" <> config ^. #postgres . #password <> "\""
  Text.putStrLn $
    "postgres_pool_size = " <>
      config ^. #postgres . #poolSize . to show . packed
  Text.putStrLn $
    "postgres_pool_timeout = " <>
      config ^. #postgres . #poolTimeout . to show . packed
  Text.putStrLn $
    "postgres_port = " <>
      config ^. #postgres . #port . to show . packed
  Text.putStrLn $
    "postgres_user = \"" <> config ^. #postgres . #user <> "\""
  Text.putStrLn "session_jwk = <JWK>"
  Text.putStrLn $
    "session_name = \"" <>
      config ^?! #session . #cookie . to sessionCookieName . utf8 <>
      "\""
  Text.putStrLn $
    "session_secure = " <>
      config ^. #session . #cookie . to cookieIsSecure . to renderIsSecure
  Text.putStrLn $
    "session_ttl = " <>
      config ^. #session . #cookie . to cookieMaxAge . to show . packed
  Text.putStrLn $
    "session_xsrf = " <>
      (case config ^. #session . #cookie . to cookieXsrfSetting of
        Nothing -> "false"
        Just _ -> "true")

  where
    renderIsSecure :: IsSecure -> Text
    renderIsSecure = \case
      Secure -> "true"
      NotSecure -> "false"

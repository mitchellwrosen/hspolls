{-# LANGUAGE ApplicativeDo #-}

module Hp.Config
  ( Config(..)
  , PostgresConfig(..)
  , readConfigFile
  , prettyPrintConfig
  ) where

import Hp.GitHub.ClientId     (GitHubClientId(..))
import Hp.GitHub.ClientSecret (GitHubClientSecret(..))

import Crypto.JOSE.JWK         (JWK)
import Data.ByteArray.Encoding (Base(..), convertFromBase)
import Data.Text.Encoding      (decodeUtf8)
import Data.Validation
import Servant.Auth.Server     (CookieSettings(..), IsSecure(..),
                                JWTSettings(..), SameSite(..),
                                defaultJWTSettings, defaultXsrfCookieSettings)

import qualified Crypto.JOSE.JWK as JWK
import qualified Data.ByteString as ByteString
import qualified Data.Text.IO    as Text
import qualified Dhall


-- | Config parsed straight from a dhall value, with no additional checks on the
-- values (e.g. the JWT must be a certain length string).
data UnvalidatedConfig
  = UnvalidatedConfig
  { gitHub :: UnvalidatedGitHubConfig
  , port :: Natural
  , postgres :: UnvalidatedPostgresConfig
  , session :: UnvalidatedSessionConfig
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
  { jwk :: Text
  , name :: Text
  , secure :: Bool
  , ttl :: Maybe Natural
  , xsrf :: Bool
  } deriving stock (Generic)
    deriving anyclass (Dhall.Interpret)

data Config
  = Config
  { gitHub :: GitHubConfig
  , port :: Natural
  , postgres :: PostgresConfig
  , session :: SessionConfig
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
  { cookieSettings :: CookieSettings
  , jwtSettings :: JWTSettings
  } deriving stock (Generic)

readConfigFile :: FilePath -> IO (Either [Text] Config)
readConfigFile path = do
  unvalidatedConfig :: UnvalidatedConfig <-
    Dhall.detailed (Dhall.input Dhall.auto (path ^. packed))

  pure (toEither (validateConfig unvalidatedConfig))

validateConfig :: UnvalidatedConfig -> Validation [Text] Config
validateConfig config = do
  postgres :: PostgresConfig <-
    validatePostgres (config ^. #postgres)

  session :: SessionConfig <-
    validateSession (config ^. #session)

  pure Config
    { gitHub =
        GitHubConfig
          { clientId =
              GitHubClientId (config ^. #gitHub . #clientId)
          , clientSecret =
              GitHubClientSecret (config ^. #gitHub . #clientSecret)
          }

      -- TODO validate port is < 2^6
    , port =
        config ^. #port

    , postgres =
        postgres

    , session =
        session
    }

validateCookieSettings ::
     UnvalidatedSessionConfig
  -> Validation [Text] CookieSettings
validateCookieSettings config =
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

validateJWTSettings ::
     UnvalidatedSessionConfig
  -> Validation [Text] JWTSettings
validateJWTSettings config = do
  jwk :: JWK <-
    validateJWK (config ^. #jwk)

  pure (defaultJWTSettings jwk)

validateSession :: UnvalidatedSessionConfig -> Validation [Text] SessionConfig
validateSession config =
  SessionConfig
    <$> validateCookieSettings config
    <*> validateJWTSettings config

validatePostgres ::
     UnvalidatedPostgresConfig
  -> Validation [Text] PostgresConfig
validatePostgres config =
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
      config ^. #session . #cookieSettings . to sessionCookieName . to decodeUtf8 <>
      "\""
  Text.putStrLn $
    "session_secure = " <>
      config ^. #session . #cookieSettings . to cookieIsSecure . to renderIsSecure
  Text.putStrLn $
    "session_ttl = " <>
      config ^. #session . #cookieSettings . to cookieMaxAge . to show . packed
  Text.putStrLn $
    "session_xsrf = " <>
      (case config ^. #session . #cookieSettings . to cookieXsrfSetting of
        Nothing -> "false"
        Just _ -> "true")

  where
    renderIsSecure :: IsSecure -> Text
    renderIsSecure = \case
      Secure -> "true"
      NotSecure -> "false"

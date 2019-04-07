module Hp.Config
  ( Config(..)
  , readConfigFile
  ) where

import Hp.GitHub.ClientId     (GitHubClientId(..))
import Hp.GitHub.ClientSecret (GitHubClientSecret(..))

import Data.Validation

import qualified Dhall


-- | Config parsed straight from a dhall value, with no additional checks on the
-- values (e.g. the JWT must be a certain length string).
data UnvalidatedConfig
  = UnvalidatedConfig
  { gitHubClientId :: Text
  , gitHubClientSecret :: Text
  , port :: Natural
  } deriving stock (Generic)
    deriving anyclass (Dhall.Interpret)

data Config
  = Config
  { gitHubClientId :: GitHubClientId
  , gitHubClientSecret :: GitHubClientSecret
  , port :: Natural
  } deriving stock (Generic, Show)

readConfigFile :: FilePath -> IO (Either [Text] Config)
readConfigFile path = do
  unvalidatedConfig :: UnvalidatedConfig <-
    Dhall.detailed (Dhall.input Dhall.auto (path ^. packed))

  pure (toEither (validateConfig unvalidatedConfig))

validateConfig :: UnvalidatedConfig -> Validation [Text] Config
validateConfig UnvalidatedConfig { gitHubClientId, gitHubClientSecret, port } =
  -- TODO validate port is < 2^6
  pure Config
    { gitHubClientId = GitHubClientId gitHubClientId
    , gitHubClientSecret = GitHubClientSecret gitHubClientSecret
    , port = port
    }

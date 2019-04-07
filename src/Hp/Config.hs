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
  { gitHub :: UnvalidatedGitHubConfig
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
validateConfig config =

  -- TODO validate port is < 2^6
  pure Config
    { gitHub =
        GitHubConfig
          { clientId =
              GitHubClientId (config ^. field @"gitHub" . field @"clientId")
          , clientSecret =
              GitHubClientSecret (config ^. field @"gitHub" . field @"clientSecret")
          }
    , port =
        config ^. field @"port"
    }

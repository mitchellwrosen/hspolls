module Hp.Config
  ( Config(..)
  , readConfigFile
  ) where

import Data.Validation

import qualified Dhall


-- | Config parsed straight from a dhall value, with no additional checks on the
-- values (e.g. the JWT must be a certain length string).
data UnvalidatedConfig
  = UnvalidatedConfig
  { port :: Natural
  } deriving stock (Generic)
    deriving anyclass (Dhall.Interpret)

data Config
  = Config
  { port :: Natural
  } deriving stock (Generic)

readConfigFile :: FilePath -> IO (Either [Text] Config)
readConfigFile path = do
  unvalidatedConfig :: UnvalidatedConfig <-
    Dhall.detailed (Dhall.input Dhall.auto (path ^. packed))

  pure (toEither (validateConfig unvalidatedConfig))

validateConfig :: UnvalidatedConfig -> Validation [Text] Config
validateConfig UnvalidatedConfig { port } =
  pure Config
    { port = port
    }

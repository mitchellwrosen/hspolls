module Hp.PostgresConfig
  ( PostgresConfig(..)
  , acquirePostgresPool
  ) where

import Dhall (Interpret)

import qualified Hasql.Connection as Hasql
import qualified Hasql.Pool       as Pool


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
    deriving anyclass (Interpret)

acquirePostgresPool :: MonadIO m => PostgresConfig -> m Pool.Pool
acquirePostgresPool PostgresConfig{..} = liftIO $ Pool.acquire settings
  where
    settings :: Pool.Settings
    settings = (fromIntegral poolSize, fromIntegral poolTimeout, connSettings)

    connSettings :: Hasql.Settings
    connSettings = Hasql.settings
      (host ^. re utf8)
      (fromIntegral port)
      (user ^. re utf8)
      (password ^. re utf8)
      (dbName ^. re utf8)

-- TODO Mitchell moved PostgresConfig into Hp.Config, now this module has a
-- werid name!
module Hp.PostgresConfig
  ( acquirePostgresPool
  ) where

import Hp.Config (PostgresConfig(..))

import qualified Hasql.Connection as Hasql
import qualified Hasql.Pool       as Pool


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

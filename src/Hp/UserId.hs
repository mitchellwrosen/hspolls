module Hp.UserId where

import Data.Aeson          (FromJSON, ToJSON)
import Servant.Auth.Server (FromJWT, ToJWT)

-- TODO encrypt user id in ToJWT
-- TODO use database user id instead of GH username
newtype UserId
  = UserId { unUserId :: Text }
  deriving stock (Show)
  deriving newtype (FromJSON, ToJSON)
  deriving anyclass (FromJWT, ToJWT)

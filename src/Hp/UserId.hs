module Hp.UserId where

import Data.Aeson          (FromJSON, ToJSON)
import Servant.Auth.Server (FromJWT, ToJWT)

-- TODO encrypt user id in ToJWT
newtype UserId
  = UserId { unUserId :: Int }
  deriving newtype (FromJSON, ToJSON)
  deriving anyclass (FromJWT, ToJWT)

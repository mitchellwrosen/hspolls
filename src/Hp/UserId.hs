module Hp.UserId where

import Data.Aeson          (FromJSON, ToJSON)
import Servant.Auth.Server (FromJWT, ToJWT)

newtype UserId
  = UserId { unUserId :: Int }
  deriving newtype (FromJSON, ToJSON)
  deriving anyclass (FromJWT, ToJWT)

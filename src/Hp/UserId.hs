module Hp.UserId where

import Data.Aeson          (FromJSON, ToJSON)
import Data.UUID           (UUID)
import Servant.Auth.Server (FromJWT, ToJWT)

newtype UserId
  = UserId { unUserId :: UUID }
  deriving stock (Show)
  deriving newtype (FromJSON, ToJSON)
  deriving anyclass (FromJWT, ToJWT)

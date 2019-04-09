module Hp.UserId
  ( UserId(..)
  , userIdDecoder
  ) where

import Data.Aeson          (FromJSON, ToJSON)
import Data.UUID           (UUID)
import Servant.Auth.Server (FromJWT, ToJWT)

import qualified Hasql.Decoders as Decoder


newtype UserId
  = UserId { unUserId :: UUID }
  deriving stock (Show)
  deriving newtype (FromJSON, ToJSON)
  deriving anyclass (FromJWT, ToJWT)

userIdDecoder :: Decoder.Value UserId
userIdDecoder =
  UserId <$> Decoder.uuid

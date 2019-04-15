module Hp.Entity.User
  ( User(..)
  , EntityId(UserId)
  , UserId
  , userIdDecoder
  , userIdEncoder
  ) where

import Hp.GitHub.UserName (GitHubUserName)
import Hp.IsEntity        (IsEntity(..))

import Data.Aeson          (FromJSON, ToJSON)
import Data.UUID           (UUID)
import Servant.Auth.Server (FromJWT(..), ToJWT(..))

import qualified Hasql.Decoders as Decoder
import qualified Hasql.Encoders as Encoder


data User
  = User
  { email :: Maybe Text
  , gitHub :: Maybe GitHubUserName
  , subscribedToPollCreated :: Bool
  } deriving stock (Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

instance IsEntity User where
  newtype EntityId User
    = UserId { unUserId :: UUID }
    deriving stock (Show)
    deriving newtype (FromJSON, ToJSON)
    deriving anyclass (FromJWT, ToJWT)

type UserId
  = EntityId User

userIdDecoder :: Decoder.Value UserId
userIdDecoder =
  UserId <$> Decoder.uuid

userIdEncoder :: Encoder.Value UserId
userIdEncoder =
  coerce Encoder.uuid


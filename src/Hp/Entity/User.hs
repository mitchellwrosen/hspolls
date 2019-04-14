module Hp.Entity.User
  ( User(..)
  , EntityId(UserId)
  , UserId
  , userIdDecoder
  , userIdEncoder
  ) where

import Hp.Entity          (Entity(..))
import Hp.GitHub.UserName (GitHubUserName)
import Hp.IsEntity        (IsEntity(..))

import Crypto.JWT          (ClaimsSet, addClaim, emptyClaimsSet,
                            unregisteredClaims)
import Data.Aeson          (FromJSON, Result(..), ToJSON(..), Value(..),
                            fromJSON, object, (.=))
import Data.UUID           (UUID)
import Servant.Auth.Server (FromJWT(..), ToJWT(..))

import qualified Hasql.Decoders as Decoder
import qualified Hasql.Encoders as Encoder


-- TODO encrypt user in ToJWT
data User
  = User
  { email :: Maybe Text
  , gitHub :: Maybe GitHubUserName
  , subscribedToPollCreated :: Bool
  } deriving stock (Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

instance FromJWT (Entity User) where
  decodeJWT :: ClaimsSet -> Either Text (Entity User)
  decodeJWT claims =
    case claims ^. unregisteredClaims . at "dat" of
      Just (Object obj)
        | Just (Success userId) <- obj ^? ix "id"   . to fromJSON
        , Just (Success user)   <- obj ^? ix "user" . to fromJSON ->
            pure (Entity (UserId userId) user)
      _ ->
        Left ""

instance IsEntity User where
  newtype EntityId User
    = UserId { unUserId :: UUID }
    deriving stock (Show)

instance ToJWT (Entity User) where
  encodeJWT :: Entity User -> ClaimsSet
  encodeJWT (Entity userId user) =
    addClaim "dat" value emptyClaimsSet
    where
      value :: Value
      value =
        object
          [ "id" .= unUserId userId
          , "user" .= toJSON user
          ]

type UserId
  = EntityId User

userIdDecoder :: Decoder.Value UserId
userIdDecoder =
  UserId <$> Decoder.uuid

userIdEncoder :: Encoder.Value UserId
userIdEncoder =
  coerce Encoder.uuid


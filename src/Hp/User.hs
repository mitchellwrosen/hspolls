module Hp.User
  ( User(..)
  , userEncoder
  , userDecoder
  ) where

import Hp.Entity          (Entity(..))
import Hp.GitHub.UserName (GitHubUserName, gitHubUserNameDecoder,
                           gitHubUserNameEncoder)
import Hp.IsEntity        (IsEntity(..))
import Hp.UserId          (UserId(..))

import Crypto.JWT          (ClaimsSet, addClaim, emptyClaimsSet,
                            unregisteredClaims)
import Data.Aeson          (FromJSON, Result(..), ToJSON(..), Value(..),
                            fromJSON, object, (.=))
import Servant.Auth.Server (FromJWT(..), ToJWT(..))

import qualified Hasql.Decoders as Decoder
import qualified Hasql.Encoders as Encoder


-- TODO encrypt user in ToJWT
data User
  = User
  { gitHub :: Maybe GitHubUserName
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
  type EntityId User
    = UserId

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

userEncoder :: Encoder.Params User
userEncoder =
  (^. #gitHub) >$< Encoder.nullableParam gitHubUserNameEncoder

userDecoder :: Decoder.Row User
userDecoder =
  User
    <$> Decoder.nullableColumn gitHubUserNameDecoder

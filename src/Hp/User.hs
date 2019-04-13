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

userEncoder :: Encoder.Params (Maybe Text, Maybe GitHubUserName)
userEncoder =
  fold
    [ view _1 >$< Encoder.nullableParam Encoder.text
    , view _2 >$< Encoder.nullableParam gitHubUserNameEncoder
    ]

userDecoder :: Decoder.Row User
userDecoder = do
  email <- Decoder.nullableColumn Decoder.text
  gitHub <- Decoder.nullableColumn gitHubUserNameDecoder
  subscribedToPollCreated <- Decoder.column Decoder.bool
  pure User{..}

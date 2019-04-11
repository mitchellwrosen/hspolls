module Hp.UserId
  ( UserId(..)
  , userIdDecoder
  , userIdEncoder
  ) where

import Data.UUID (UUID)

import qualified Hasql.Decoders as Decoder
import qualified Hasql.Encoders as Encoder


newtype UserId
  = UserId { unUserId :: UUID }
  deriving stock (Show)

userIdDecoder :: Decoder.Value UserId
userIdDecoder =
  UserId <$> Decoder.uuid

userIdEncoder :: Encoder.Value UserId
userIdEncoder =
  coerce Encoder.uuid
